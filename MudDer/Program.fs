﻿open System

open MudDer.Schema
open MudDer.Movement
// See the 'F# Tutorial' project for more help.


//let monsterEncounter<'T when 'T :> IRandomizer>  (rnd:'T) = 
let generateMonsters (rnd : int -> int) = 
    [
        {Monster.Health= (rnd(1)) * 1<Health> + 1<Health>; Dmg = Die.D4; Name="Goblin"}
        {Monster.Health= (rnd(1)) * 1<Health> + 4<Health>; Dmg = Die.D6; Name="HobGoblin"}
    ] 

let getRandomMonster (rnd : int -> int) monsters = 
    monsters |> Array.ofSeq |> (fun array -> array.[rnd(array.Length)])

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let rnd = Random()
    let rng (d:Die) = rnd.Next(int(d)) + 1
    let map = 
        [
            [ 1;0;1;0;1;0;0;1;1]
            [ 1;1;1;0;1;0;0;0;1]
            [ 0;1;0;1;1;1;1;0;1]
            [ 1;1;1;1;0;1;1;1;1]
            [ 1;0;1;0;0;0;0;0;1]
            [ 1;0;1;0;0;0;0;0;1]
            [ 0;0;1;0;1;1;1;1;1;1;1]
            [ 1;1;1;0;1;0;0;0;1]
        ]

    let monsterChanceEvent monsters state = 
        if state.Monster.IsNone then
            let roll = rnd.Next(4)
            printfn "Checking for monsters, rolled a %A" roll
            if roll = 3 then
                let newState = {state with Monster = Some <| getRandomMonster rnd.Next monsters}
                printfn "A %s wanders in!" newState.Monster.Value.Name
                newState
            else state
        else state

    let monsterAttack state: Reply option * State =
        if state.Monster.IsNone then None,state 
        else
            let monsterDamage = 1<Health> * (rng(state.Monster.Value.Dmg))
            let playerHealth = state.Health -  monsterDamage
            let result = { state with Health = playerHealth }
            if playerHealth > 1<Health> then
                printfn "The Monster hits you for %A!"  monsterDamage
                None, result
            else
                let reply = Death <| sprintf "The monster hit for %A which was far too hard for your little head." monsterDamage
                Some reply,result

    let playerAttack state : State = 
        if state.Monster.IsNone then printfn "You're attacking the darkness. It just stands there, darkly."; state 
        else
            let playerDamage = 1<Health> * rng state.Damage
            let monsterHealth = state.Monster.Value.Health - playerDamage

            if monsterHealth <= 0<_> then
                let result = {state with Xp = state.Xp + 1; MoveCount = state.MoveCount+1; Monster = None }
                printfn "you killed a %A" state.Monster.Value.Name
                // sprintf " You hit %s for %A damage and have slain it" initialState.Monster.Value.Name playerDamage
                result
            else
                printfn "you hit for %A" playerDamage
                { state with Monster = Some {state.Monster.Value with Health = monsterHealth }}

    let removeMonster state = {state with Monster = None}

    let processCommand monsters cmd initialState : Reply*State = 
        let replyCant () = Msg "I'm afraid I can't let you go that way dave"
        let msg (state:State) s = Msg s, {state with MoveCount = state.MoveCount + 1}
        match cmd with
                    | Move dir -> 
                        let replyOpt,postEventState = moveEvent replyCant monsters dir initialState playerAttack monsterAttack map (checkArrivalEvent rng ) monsterChanceEvent removeMonster rng 
                        if replyOpt.IsSome then 
                            replyOpt.Value,postEventState |> monsterChanceEvent monsters
                        else
                            // did not move
                            msg postEventState <| sprintf "(noreply)moved to (%A, %A)" postEventState.X postEventState.Y
                    
                    | Wait -> 
                        if initialState.Monster.IsSome then 
                            msg initialState <| sprintf "The %A would like to finish, what are you waiting for?" initialState.Monster.Value.Name 
                        else msg initialState "What are you waiting for? This dungeon isn't going to explore itself"
                    | Attack -> 
                        let replyOpt,combatResolvedState = initialState |> (playerAttack >> monsterAttack)
                        if replyOpt.IsSome then
                            replyOpt.Value,combatResolvedState
                        else
                            msg combatResolvedState "and now?"
                        
    let mailbox = 
        let processor = new MailboxProcessor<Message>(fun inbox ->
            let monsters = generateMonsters rnd.Next
            let commandProcessor = processCommand monsters
            let rec loop() = 
                async{
                    
                    let! message = (* printfn"waiting for nextMessage"; *) inbox.Receive()
                    let cmd, initialState, replyChannel = message
                    let reply,state = 
                        try
                            commandProcessor cmd initialState
                        with ex -> Exception (cmd, ex), initialState

                    //let msg (rc:AsyncReplyChannel<Reply*State>) (state:State) s = rc.Reply(Msg s, {state with MoveCount = state.MoveCount + 1})
                    //let replyCant cantstate = replyChannel.Reply <| (Msg "I'm afraid I can't let you go that way dave",cantstate)
                    replyChannel.Reply (reply,state)
                    do! loop()
                }

            loop())
        processor.Start()
        processor
    let rec takeInput (state:State) (s:string) : bool*State = 

        let op (command:Command)  = Some <| fun replyChannel -> ( command ,state,replyChannel)
        let inputMap = match s.ToLowerInvariant() with 
                                                                | "west" -> op <| Command.Move Move.West // Some <| fun replyChannel -> Command.Move Move.West, state,replyChannel
                                                                | "east" -> op <| Command.Move Move.East
                                                                | "north" -> op <| Command.Move Move.North
                                                                | "south" -> op <| Command.Move Move.South
                                                                | "stats" -> printfn "Health: %A, Xp:%A, X:%A, Y:%A" state.Health state.Xp state.X state.Y; op Wait
                                                                | "quit" | "exit" | "q" -> (* printfn"found quit"; *) None
                                                                | "attack" -> op Attack
                                                                |_ -> (* printfn "no op";*) op Wait
        match inputMap with 
        |Some msg -> 
                    let reply,newState  = mailbox.PostAndReply inputMap.Value
                    match reply,newState with
                               | RoomChange,_ -> true,newState
                               | Msg s,_ -> printfn "%s" s; true, newState
                               | Exception (cmd, ex), newState -> printfn "Failed to process cmd '%A' input exception was %A" cmd ex;  false, newState
                               | _ -> true, newState
        |None -> false,state

    let rec msgPump (state:State):State option = 
        //printfn "unfolding!"
        let shouldContinue,newState = printfn "Command?"; takeInput state <| Console.ReadLine()
        //Console.Out.Flush()
        //System.Threading.Thread.Sleep(100)
        if shouldContinue then (* printfn "continuing adventure!"; *) msgPump newState
        else printfn "quitting!"; None
    //printfn "Preparing to unfold!"
    let initialState = State.Initial
    msgPump initialState |> ignore
    printfn "msgPump finished, waiting for any key to exit"
    Console.ReadKey() |> ignore
    0 // return an integer exit code


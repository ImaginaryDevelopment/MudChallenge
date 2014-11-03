open System

type Die=
    | D2 = 2
    | D4 = 4
    | D6 = 6
    | D8 = 8
    | D10 = 10
    | D20 = 20
    //with 
    //member x.Roll (rng:int->int) = rng(x)
[<Measure>]
type Health

// See the 'F# Tutorial' project for more help.
type Monster = { Health:int<Health>; Name:string; (* THAC0: byte;*) Dmg: Die (* *byte seq *) }

type State = { Room:string; Xp:int; Level:int; X:int; Y:int; MoveCount:int; Monster:Monster option; Health: int<Health>; Damage: Die} with
    static member Initial = {Room="start"; Xp=0; Level =1; X=0; Y=0;Health = 4<Health>; MoveCount =0; Monster = None; Damage = Die.D4}

type Treasure =
    |Health of Die
    |Xp of Die

type Event =
    | ArrivalMessage of string
    //| MonsterArrival of Monster
    | TreasureFound of Treasure

type Move =
    |West
    |East
    |North
    |South

type Command = 
    |Move of Move
    |Wait
    |Attack

//type Message = 
//        |Query of AsyncReplyChannel<Reply>
//        |Update of AccountUpdate*AsyncReplyChannel<Reply>
// recieve = match message with 
//                    | Query replyChannel -> 
//                        replyChannel.Reply(Info(balance))
//                    | (Update(AccountUpdate.BalanceChange(changeAmount),replyChannel)) ->
// post = 
//    mailbox.PostAndReply( fun replyChannel -> Query replyChannel)
//    mailbox.PostAndReply( fun replyChannel -> Update(Open(doOpen),replyChannel))
type Message = Command*State*AsyncReplyChannel<Reply*State>
    //|East of State*AsyncReplyChannel<Reply*State>
and
    Reply =
    | RoomChange
    | Msg of string
    | Death of string
    | Exception of Command*Exception

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
        ]

    //static events
    let events = 
        [ 
            0,1, Event.ArrivalMessage ("Welcome to the jungle.") // test message to see if they are working
        ]

    let checkArrivalEvent state : State = 
        printfn "checking for an arrival event"
        let event = events |> Seq.tryFind(fun (x,y,_) -> x = state.X && y = state.Y )
        if event.IsSome then
            let _,_,info = event.Value
            match info with
            | ArrivalMessage s -> printfn "Arriving at LaGuardia, just kidding it's %s" s; state
            | TreasureFound treasure -> 
                match treasure with
                |Xp d -> 
                    let xp = rng d
                    printfn "Found Artifact worth %i experience " xp
                    {state with Xp= state.Xp + xp}
                |Health d ->
                    let health = rng d * 1<Health>
                    printfn "Found Magic potion, healed for %A " health
                    {state with Health= state.Health + health}
        else
            state

        //let movedState = {state with Room = "another sad room title"; X=state.X+1; MoveCount= state.MoveCount + 1}
    let escapeMonsterMove state cant (playerAttack:State->State) (monsterAttack:State -> Reply option * State): Reply option * State = // reply if the escape attempt is blocked
        if state.Monster.IsSome then
            match rng Die.D6 with
            | 1 | 2  -> None,state // escape
            | 3 | 4 -> Msg <| sprintf "Your escape attempt is blocked by %s" state.Monster.Value.Name |> (fun r -> Some r,state)
            | 5 -> 
                monsterAttack state
            | 6 -> 
                let newState = playerAttack state // Riposte
                None, newState
            | _ -> failwithf "escape monster move failure, out of bounds"
        else None,state

    let monsterChanceEvent state = 
        if state.Monster.IsNone then
            let roll = rnd.Next(4)
            printfn "Checking for monsters, rolled a %A" roll
            if roll = 3 then
                let newState = {state with Monster= Some {Monster.Health= (rnd.Next(1)) * 1<Health> + 1<Health>; Dmg = Die.D2; Name="Goblin"}}
                printfn "A %s wanders in!" newState.Monster.Value.Name
                newState
            else state
        else state

    let monsterAttack state: Reply option * State =
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
        let playerDamage = 1<Health> * rng state.Damage
        let monsterHealth = state.Monster.Value.Health - playerDamage

        if monsterHealth <= 0<_> then
            let result = {state with Xp = state.Xp + 1; MoveCount = state.MoveCount+1; Monster = None }
            result
        else
            { state with Monster = Some {state.Monster.Value with Health = monsterHealth }} // <| sprint " You hit %s for %A damage."

    let moveEvent move initialState cant : Reply option * State = 
        let replyOpt,state = if initialState.Monster.IsSome then escapeMonsterMove initialState cant playerAttack monsterAttack else None,initialState
        let condition,movedState = 
            match move with
            |West -> state.X>0, {state with Room = "another sad room title"; X=state.X-1; MoveCount= state.MoveCount + 1}
            |East -> state.X < map.[state.Y].Length, {state with Room = "another sad room title"; X=state.X+1; MoveCount= state.MoveCount + 1}
            |North -> state.Y > 0, {state with Room = "A northern looking room"; Y = state.Y-1;MoveCount = state.MoveCount+1}
            |South -> state.Y+1< Seq.length map, {state with Room = "A southern looking room"; Y = state.Y+1;MoveCount = state.MoveCount+1}

        if condition && map.[movedState.Y].[movedState.X] = 1 then 
            printfn "moved to (%A, %A)" movedState.X movedState.Y
            None,movedState |> checkArrivalEvent |> monsterChanceEvent
        else 
            (Some (cant())),state
    let processCommand cmd initialState : Reply*State = 
        let replyCant () = Msg "I'm afraid I can't let you go that way dave"
        let msg (state:State) s = Msg s, {state with MoveCount = state.MoveCount + 1}
        match cmd with
                    | Move dir -> 
                        let replyOpt,postEventState = moveEvent dir initialState replyCant
                        if replyOpt.IsSome then 
                            replyOpt.Value,postEventState |> monsterChanceEvent
                        else
                            // did not move
                            msg postEventState <| sprintf "(noreply)moved to (%A, %A)" postEventState.X postEventState.Y
                    
                    | Wait -> msg initialState "What are you waiting for? This dungeon isn't going to explore itself"
                    | Attack -> 
                        if initialState.Monster.IsSome then 
                            let monsterHealth = initialState.Monster.Value.Health - 1<Health> * rng initialState.Damage
                            if monsterHealth <= 0<_> then
                                let result = {initialState with Xp = initialState.Xp + 1; MoveCount = initialState.MoveCount+1; Monster = None }
                                msg result <| sprintf " You hit %s and have slain it" initialState.Monster.Value.Name
                            else
                                let replyOpt,state = monsterAttack initialState
                                if replyOpt.IsSome then
                                    replyOpt.Value,state
                                else
                                    msg state "and now"
                        else
                            msg initialState "You're attacking the darkness. It just stands there, darkly."
                                
                        
    let mailbox = 
        let processor = new MailboxProcessor<Message>(fun inbox ->
            let rec loop() = 
                async{
                    
                    let! message = (* printfn"waiting for nextMessage"; *) inbox.Receive()
                    let cmd, initialState, replyChannel = message
                    let reply,state = 
                        try
                            processCommand cmd initialState
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
                                                                | "quit" | "exit" -> (* printfn"found quit"; *) None
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


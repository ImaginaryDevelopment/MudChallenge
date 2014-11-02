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

type Event =
    | ArrivalMessage of string
    | MonsterArrival of Monster
    | TreasureFound of string

type Command = 
    |West
    |East
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

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let rnd = Random()
    let rng (d:Die) = rnd.Next(int(d)) // let 0 model a miss for now
    let map = 
        [
            [ 1;0;1;0;1;0;0;1;1]
            [ 1;1;1;0;1;0;0;0;1]
            [ 0;1;0;1;1;1;1;0;1]
            [ 1;1;1;1;0;1;1;1;1]
        ]
    let events = 
        [ 
            0,0, Event.ArrivalMessage ("Welcome to the jungle.") // test message to see if they are working
        ]

    let mailbox = 
        let processor = new MailboxProcessor<Message>(fun inbox ->
            let rec loop() = 
                async{
                    
                    let! message = (* printfn"waiting for nextMessage"; *) inbox.Receive()

                    let msg (rc:AsyncReplyChannel<Reply*State>) (state:State) s = rc.Reply(Msg s, {state with MoveCount = state.MoveCount + 1})
                    match message with
                    | (West,state,replyChannel) -> replyChannel.Reply( RoomChange,{state with Room = "west"})
                    | (East,state,replyChannel) -> replyChannel.Reply( if state.Room="west" then printfn "going west"; RoomChange,{state with Room = "start"} else Msg "Can't go that way dave",state)
                    | (Wait,state,replyChannel) -> msg replyChannel state "What are you waiting for? This dungeon isn't going to explore itself"
                    | (Attack,state,replyChannel) -> 
                        if state.Monster.IsSome then 
                            let monsterHealth = state.Monster.Value.Health - 1<Health> * rng state.Damage
                            if monsterHealth <= 0<_> then
                                let result = {state with Xp = state.Xp + 1; MoveCount = state.MoveCount+1; Monster = None }
                                msg replyChannel result <| sprintf " You hit %s and have slain it" state.Monster.Value.Name
                            else
                                let monsterDamage = 1<Health> * (rng(state.Monster.Value.Dmg))
                                let playerHealth = state.Health -  monsterDamage
                                let result = { state with Health = playerHealth }
                                if playerHealth > 1<Health> then
                                    msg replyChannel result "The Monster attacks you!"  //counter attack
                                else
                                    replyChannel.Reply( Death <| sprintf "The monster hit for %A which was far too hard for your little head." monsterDamage,result)
                        else
                            msg replyChannel state "You're attacking the darkness. It just stands there, darkly."

                    do! loop()
                }

            loop())
        processor.Start()
        processor
    let rec takeInput (state:State) (s:string) : bool*State = 

        let op command  = Some <| fun replyChannel -> ( command ,state,replyChannel)
        let inputMap = match s.ToLowerInvariant() with 
                                                                | "west" -> Some <| fun replyChannel -> Command.West, state,replyChannel
                                                                | "east" -> op East
                                                                | "quit" | "exit" -> (* printfn"found quit"; *) None
                                                                | "attack" -> op Attack
                                                                |_ -> (* printfn "no op";*) op Wait
        match inputMap with 
        |Some msg -> 
                    let reply,newState  = mailbox.PostAndReply inputMap.Value
                    match reply,newState with
                               | RoomChange,_ -> true,newState
                               | Msg s,_ -> printfn "%s" s; true, newState
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


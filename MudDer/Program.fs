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

// See the 'F# Tutorial' project for more help.
type Monster = { Health:int; Name:string; (* THAC0: byte;*) Dmg: Die*byte seq}

type State = { Room:string; Xp:int; Level:int; X:int; Y:int; MoveCount:int; Monster:Monster option; Damage: Die} with
    static member Initial = {Room="start"; Xp=0; Level =1; X=0; Y=0; MoveCount =0; Monster = None; Damage = Die.D4}
type Event =
    | ArrivalMessage of string
    | MonsterArrival of Monster
    | TreasureFound of string

type Command =
    |West
    |East
    |Wait
    |Attack

type RequestResponse = State*AsyncReplyChannel<Reply*State>
and
    Message = 
    |Command of Command*RequestResponse
and
    Reply =
    | RoomChange
    | Message of string
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
                    let msg (rc:AsyncReplyChannel<Reply*State>) (state:State) s = rc.Reply(Message s, {state with MoveCount = state.MoveCount + 1})
                    match message with
                    | Command (West, (state,replyChannel)) -> replyChannel.Reply( RoomChange,{state with Room = "west"})
                    | Command (East, (state,replyChannel)) -> replyChannel.Reply( if state.Room="west" then printfn "going west"; RoomChange,{state with Room = "start"} else Message "Can't go that way dave",state)
                    | Command (Wait, (state,replyChannel)) -> msg replyChannel state "What are you waiting for? This dungeon isn't going to explore itself"
                    | Command (Attack, (state,replyChannel)) -> 
                        if state.Monster.IsSome then 
                            let monsterHealth = state.Monster.Value.Health - rng state.Damage
                            if monsterHealth <= 0 then
                                let result = {state with Xp = state.Xp + 1; MoveCount = state.MoveCount+1; Monster = None }
                                msg replyChannel result <| sprintf " You hit %s and have slain it" state.Monster.Value.Name
                        else
                            msg replyChannel state "The Monster attacks you!"                                        //counter attack

                    do! loop()
                }

            loop())
        processor.Start()
        processor
    let rec takeInput (rng:int->int) (state:State) (s:string) : bool*State = 
        // printfn "Found input! %s" s
        let op funCtor =Some <| fun replyChannel -> funCtor(state,replyChannel)
        let inputMap = match s.ToLowerInvariant() with 
                                                                | "west" -> Some <| fun replyChannel -> (West,(state,replyChannel))

        match inputMap with 
        |Some msg -> 
                    let reply,newState  = mailbox.PostAndReply inputMap.Value
                    match reply,newState with
                               | RoomChange,_ -> true,newState
                               | Message s,_ -> printfn "%s" s; true, newState
        |None -> false,state

    let rec msgPump (state:State):State option = 
        //printfn "unfolding!"
        let shouldContinue,newState = printfn "Command?"; takeInput state (Console.ReadLine())
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


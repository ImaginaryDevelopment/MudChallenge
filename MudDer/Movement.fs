module MudDer.Movement

open MudDer.Schema

let events = 
    [ 
        0,1, Event.ArrivalMessage ("Welcome to the jungle.") // test message to see if they are working
    ]

let checkArrivalEvent (rng: Die->int) state : State = 
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

let escapeMonsterMove state cant (playerAttack:State->State) (monsterAttack:State -> Reply option * State) rng : Reply option * State = // reply if the escape attempt is blocked
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

let moveEvent cant monsters move initialState playerAttack monsterAttack (map:int list list) checkArrivalEvent monsterChanceEvent removeMonster rng: Reply option * State = 
        let replyOpt,state = if initialState.Monster.IsSome then escapeMonsterMove initialState cant playerAttack monsterAttack rng else None,initialState
        if replyOpt.IsSome then replyOpt,state else
            let condition,movedState = 
                match move with
                |West -> state.X>0, {state with Room = "another sad room title"; X=state.X-1; MoveCount= state.MoveCount + 1;} |> removeMonster
                |East -> state.X < map.[state.Y].Length, {state with Room = "another sad room title"; X=state.X+1; MoveCount= state.MoveCount + 1}|> removeMonster
                |North -> state.Y > 0, {state with Room = "A northern looking room"; Y = state.Y-1;MoveCount = state.MoveCount+1}|> removeMonster
                |South -> state.Y+1< Seq.length map, {state with Room = "A southern looking room"; Y = state.Y+1;MoveCount = state.MoveCount+1}|> removeMonster

            if condition && map.[movedState.Y].[movedState.X] = 1 then 
                if state.Monster.IsSome then printfn "You managed to escape the %A" state.Monster.Value.Name
                printfn "moved to (%A, %A)" movedState.X movedState.Y
                None,movedState |> checkArrivalEvent |> monsterChanceEvent monsters
            else 
                (Some (cant())),state

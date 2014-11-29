module MudDer.Schema
[<Measure>]
type Health

type Die=
    | D2 = 2
    | D4 = 4
    | D6 = 6
    | D8 = 8
    | D10 = 10
    | D20 = 20

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
    |Load of State

type Reply =
    | RoomChange
    | Msg of string
    | Death of string
    | Exception of Command * System.Exception

type Message = Command*State*AsyncReplyChannel<Reply*State>
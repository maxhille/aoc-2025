module Puzzle exposing
    ( Part
    , Puzzle
    , Step(..)
    , compute
    , execute
    , initPart
    , part
    , result
    , step
    , viewPart
    )

import Interface as IF
import Parser exposing (Parser)


type Step state
    = Loop state
    | Done state
    | Error String


type alias Puzzle =
    { parts : List Part
    }


type Part
    = Part PartIF


type alias PartIF =
    { view : () -> List String
    , result : () -> String
    , init : String -> Part
    , step : Int -> Part
    }


viewPart : Part -> List String
viewPart (Part p) =
    p.view ()


initPart : String -> Part -> Part
initPart input (Part p) =
    p.init input


step : Int -> Part -> Part
step steps (Part p) =
    p.step steps


result : Part -> String
result (Part p) =
    p.result ()


compute : Part -> String -> String
compute (Part p) input =
    let
        (Part inited) =
            p.init input

        (Part computed) =
            inited.step -1
    in
    computed.result ()


execute : (state -> Step state) -> state -> Result String state
execute stepFn state =
    case stepHelp stepFn (Loop state) -1 of
        Done state2 ->
            Ok state2

        Loop _ ->
            Err "function did not halt"

        Error str ->
            Err str


part : { view : state -> List String, result : state -> String, parser : Parser a, init : a -> state, step : state -> Step state } -> Part
part i =
    impl
        { view =
            \step_ ->
                case step_ of
                    Error str ->
                        [ "error: " ++ str ]

                    Loop state ->
                        i.view state

                    Done state ->
                        i.view state
        , result =
            \step_ ->
                case step_ of
                    Error str ->
                        "error: " ++ str

                    Loop _ ->
                        "looping..."

                    Done state ->
                        i.result state
        , init =
            \_ input ->
                case Parser.run i.parser input of
                    Err error ->
                        Error (Parser.deadEndsToString error)

                    Ok a ->
                        Loop <| i.init a
        , step = stepHelp i.step
        }
        (Error "No input!")


stepHelp : (state -> Step state) -> Step state -> Int -> Step state
stepHelp fn step_ left =
    case step_ of
        Loop state2 ->
            if left == -1 then
                stepHelp fn (fn state2) -1

            else if left == 0 then
                Loop state2

            else
                stepHelp fn (fn state2) (left - 1)

        Done state2 ->
            Done state2

        Error str ->
            Error str


impl :
    { view : rep -> List String
    , result : rep -> String
    , init : rep -> String -> rep
    , step : rep -> Int -> rep
    }
    -> rep
    -> Part
impl cons =
    IF.impl PartIF
        |> IF.add (\rep () -> cons.view rep)
        |> IF.add (\rep () -> cons.result rep)
        |> IF.wrap (\raise rep input -> raise (cons.init rep input))
        |> IF.wrap (\raise rep steps -> raise (cons.step rep steps))
        |> IF.map Part
        |> IF.init (\raise rep -> raise rep)

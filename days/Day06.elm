module Day06 exposing (Op(..), parser, part1, puzzle)

import Parser exposing ((|.), (|=), Parser, Trailing(..), problem)
import Puzzle exposing (Puzzle, Step(..))


type Op
    = Add
    | Mul


type alias Problem =
    { numbers : List Int
    , op : Op
    }


type alias State =
    { problems : List Problem
    , sum : Int
    }


part1 : Puzzle.Part
part1 =
    Puzzle.part
        { view = always []
        , result = .sum >> String.fromInt
        , parser = parser |> Parser.andThen transposeWorksheet
        , init = \problems -> { problems = problems, sum = 0 }
        , step = solveProblems
        }


solveProblems : State -> Step State
solveProblems { problems, sum } =
    case problems of
        [] ->
            Done { problems = problems, sum = sum }

        problem :: rest ->
            let
                result =
                    case problem.op of
                        Add ->
                            List.sum problem.numbers

                        Mul ->
                            List.product problem.numbers
            in
            Loop { problems = rest, sum = sum + result }


transposeWorksheet : { numbers : List (List Int), ops : List Op } -> Parser (List Problem)
transposeWorksheet { numbers, ops } =
    let
        maybe =
            List.head numbers
                |> Maybe.map
                    (List.indexedMap
                        (\i _ ->
                            List.map (\row -> List.drop i row |> List.head |> Maybe.withDefault 0) numbers
                        )
                    )
    in
    case maybe of
        Nothing ->
            Parser.problem "could not collect problem numbers"

        Just problemNumbers ->
            Parser.succeed (List.map2 Problem problemNumbers ops)


parser : Parser { numbers : List (List Int), ops : List Op }
parser =
    let
        help state =
            Parser.oneOf
                [ Parser.succeed (\int -> Parser.Loop { state | revInts = int :: state.revInts })
                    |= Parser.int
                , Parser.succeed
                    (Parser.Loop
                        { state
                            | revIntss =
                                if state.revInts == [] then
                                    state.revIntss

                                else
                                    List.reverse state.revInts :: state.revIntss
                            , revInts = []
                        }
                    )
                    |. Parser.symbol "\n"
                , Parser.succeed (Parser.Loop { state | revOps = Mul :: state.revOps })
                    |. Parser.symbol "*"
                , Parser.succeed (Parser.Loop { state | revOps = Add :: state.revOps })
                    |. Parser.symbol "+"
                , Parser.succeed (Parser.Loop state)
                    |. Parser.symbol " "
                , Parser.succeed
                    (Parser.Done
                        { numbers = List.reverse state.revIntss
                        , ops = List.reverse state.revOps
                        }
                    )
                    |. Parser.end
                ]
    in
    Parser.loop { revInts = [], revIntss = [], revOps = [] } help


puzzle : Puzzle
puzzle =
    { parts = [ part1 ] }

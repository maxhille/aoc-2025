module Day06 exposing (Op(..), parser1, parser2, part1, part2, problemParser2, puzzle)

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
        , parser = parser1 |> Parser.andThen transposeWorksheet
        , init = \problems -> { problems = problems, sum = 0 }
        , step = solveProblems
        }


part2 : Puzzle.Part
part2 =
    Puzzle.part
        { view = always []
        , result = .sum >> String.fromInt
        , parser = parser2
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


parser1 : Parser { numbers : List (List Int), ops : List Op }
parser1 =
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


parser2 : Parser (List Problem)
parser2 =
    charsParser
        |> Parser.andThen
            (\chars ->
                let
                    transposed =
                        chars
                            |> transpose
                            |> List.reverse
                            |> List.map String.fromList
                            |> String.join "\n"
                in
                case Parser.run problemParser2 transposed of
                    Ok problems ->
                        Parser.succeed problems

                    Err error ->
                        Parser.problem (Parser.deadEndsToString error)
            )


transpose : List (List Char) -> List (List Char)
transpose lists =
    List.head lists
        |> Maybe.map
            (List.indexedMap
                (\i _ ->
                    List.map (\row -> List.drop i row |> List.head |> Maybe.withDefault ' ') lists
                )
            )
        |> Maybe.withDefault [ [] ]


problemParser2 : Parser (List Problem)
problemParser2 =
    let
        help state =
            Parser.oneOf
                [ Parser.succeed
                    (Parser.Loop
                        { state
                            | revProblems =
                                { numbers = List.reverse state.revNumbers
                                , op = Mul
                                }
                                    :: state.revProblems
                            , revNumbers = []
                        }
                    )
                    |. Parser.symbol "*"
                , Parser.succeed
                    (Parser.Loop
                        { state
                            | revProblems =
                                { numbers = List.reverse state.revNumbers
                                , op = Add
                                }
                                    :: state.revProblems
                            , revNumbers = []
                        }
                    )
                    |. Parser.symbol "+"
                , Parser.succeed
                    (\int ->
                        Parser.Loop
                            { state
                                | revNumbers = int :: state.revNumbers
                            }
                    )
                    |= Parser.int
                , Parser.succeed (Parser.Loop state)
                    |. Parser.symbol "\n"
                , Parser.succeed (Parser.Loop state)
                    |. Parser.symbol " "
                , Parser.succeed (Parser.Done <| List.reverse state.revProblems)
                    |. Parser.end
                ]
    in
    Parser.loop { revNumbers = [], revProblems = [] } help


charsParser : Parser (List (List Char))
charsParser =
    let
        help : { revChars : List Char, revLines : List (List Char) } -> Parser (Parser.Step { revChars : List Char, revLines : List (List Char) } (List (List Char)))
        help state =
            Parser.oneOf
                [ Parser.succeed (\c -> Parser.Loop { state | revChars = c :: state.revChars })
                    |= charParser (\c -> Char.isDigit c || c == ' ' || c == '*' || c == '+')
                , Parser.succeed (Parser.Loop { state | revChars = [], revLines = List.reverse state.revChars :: state.revLines })
                    |. Parser.symbol "\n"
                , Parser.succeed
                    (Parser.Done <|
                        List.reverse
                            (if state.revChars == [] then
                                state.revLines

                             else
                                List.reverse state.revChars :: state.revLines
                            )
                    )
                    |. Parser.end
                ]
    in
    Parser.loop { revChars = [], revLines = [] } help


charParser : (Char -> Bool) -> Parser Char
charParser fn =
    Parser.succeed ()
        |. Parser.chompIf fn
        |> Parser.getChompedString
        |> Parser.andThen
            (\str ->
                case String.uncons str of
                    Just ( c, _ ) ->
                        Parser.succeed c

                    Nothing ->
                        Parser.problem "could not parse char"
            )


puzzle : Puzzle
puzzle =
    { parts = [ part1, part2 ] }

module Day03 exposing (bankParser, highest, parser, part1, part2, puzzle)

import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle, Step(..))
import Shared


type alias Bank =
    List Int


type alias State =
    { banks : List Bank
    , joltage : Int
    }


part1 : Puzzle.Part
part1 =
    Puzzle.part
        { view = view
        , result = .joltage >> String.fromInt
        , parser = parser
        , init = \banks -> { banks = banks, joltage = 0 }
        , step = total 2
        }


part2 : Puzzle.Part
part2 =
    Puzzle.part
        { view = view
        , result = .joltage >> String.fromInt
        , parser = parser
        , init = \banks -> { banks = banks, joltage = 0 }
        , step = total 12
        }


view : State -> List String
view { banks, joltage } =
    [ "banks: " ++ String.fromInt (List.length banks)
    , "joltage: " ++ String.fromInt joltage
    ]


total : Int -> State -> Step State
total amount { banks, joltage } =
    case banks of
        bank :: rest ->
            Loop
                { banks = rest
                , joltage = joltage + highest amount bank
                }

        [] ->
            Done { banks = [], joltage = joltage }


highest : Int -> Bank -> Int
highest amount bank =
    highestHelp [] amount bank
        |> List.indexedMap (\i joltage -> 10 ^ i * joltage)
        |> List.sum


highestHelp : Bank -> Int -> Bank -> Bank
highestHelp turned amount bank =
    let
        boundedMaximum =
            List.reverse bank
                |> List.drop (amount - 1)
                |> List.maximum
                |> Maybe.withDefault 0
    in
    if amount == 0 then
        turned

    else
        case dropUntilFirst boundedMaximum bank of
            joltage :: rest ->
                highestHelp (joltage :: turned) (amount - 1) rest

            [] ->
                turned


dropUntilFirst : Int -> List Int -> List Int
dropUntilFirst first ints =
    case ints of
        int :: rest ->
            if int == first then
                ints

            else
                dropUntilFirst first rest

        [] ->
            []


parser : Parser (List (List Int))
parser =
    Shared.linesParser bankParser


bankParser : Parser (List Int)
bankParser =
    let
        help revInts =
            Parser.oneOf
                [ Parser.getChompedString (Parser.chompIf Char.isDigit)
                    |> Parser.andThen
                        (\c ->
                            case String.toInt c of
                                Nothing ->
                                    Parser.problem <| c ++ " is not a digit"

                                Just n ->
                                    Parser.succeed (Parser.Loop (n :: revInts))
                        )
                , Parser.succeed ()
                    |> Parser.map (\_ -> Parser.Done (List.reverse revInts))
                ]
    in
    Parser.loop [] help


puzzle : Puzzle
puzzle =
    { parts = [ part1, part2 ] }

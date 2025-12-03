module Day03 exposing (bankParser, highest, parser, part1, puzzle)

import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle, Step(..))


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
        , step = total
        }


view : State -> List String
view { banks, joltage } =
    [ "banks: " ++ String.fromInt (List.length banks)
    , "joltage: " ++ String.fromInt joltage
    ]


total : State -> Step State
total { banks, joltage } =
    case banks of
        bank :: rest ->
            Loop
                { banks = rest
                , joltage = joltage + highest bank
                }

        [] ->
            Done { banks = [], joltage = joltage }


highest : Bank -> Int
highest =
    highestHelp { max1 = 0, max2 = 0 }


highestHelp : { max1 : Int, max2 : Int } -> Bank -> Int
highestHelp { max1, max2 } bank =
    case bank of
        [] ->
            max1 * 10 + max2

        lastJoltage :: [] ->
            if lastJoltage > max2 then
                highestHelp { max1 = max1, max2 = lastJoltage } bank

            else
                highestHelp { max1 = max1, max2 = max2 } []

        joltage1 :: joltage2 :: rest ->
            if joltage1 > max1 then
                highestHelp { max1 = joltage1, max2 = joltage2 } (joltage2 :: rest)

            else if joltage1 > max2 then
                highestHelp { max1 = max1, max2 = joltage1 } (joltage2 :: rest)

            else
                highestHelp { max1 = max1, max2 = max2 } (joltage2 :: rest)


parser : Parser (List (List Int))
parser =
    linesParser bankParser


linesParser : Parser a -> Parser (List a)
linesParser itemParser =
    Parser.loop [] (linesParserHelp itemParser)


linesParserHelp : Parser a -> List a -> Parser (Parser.Step (List a) (List a))
linesParserHelp itemParser revItems =
    Parser.oneOf
        [ Parser.succeed ()
            |. Parser.end
            |> Parser.map (\_ -> Parser.Done (List.reverse revItems))
        , Parser.succeed (\_ -> Parser.Loop revItems)
            |= Parser.symbol "\n"
        , Parser.succeed (\stmt -> Parser.Loop (stmt :: revItems))
            |= itemParser
        ]


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
    { parts = [ part1 ] }

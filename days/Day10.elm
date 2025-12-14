module Day10 exposing (Button, Light(..), Machine, fewestButtons, parser, part1, puzzle, seq)

import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle, Step(..))
import Shared


type Light
    = On
    | Off


type alias Button =
    List Int


type alias Machine =
    { lights : List Light
    , buttons : List Button
    , joltages : List Int
    }


type alias State =
    { machines : List Machine
    , presses : Int
    }


part1 : Puzzle.Part
part1 =
    Puzzle.part
        { view = always []
        , result = .presses >> String.fromInt
        , parser = parser
        , init = \machines -> { machines = machines, presses = 0 }
        , step = configurations
        }


configurations : State -> Step State
configurations state =
    case state.machines of
        [] ->
            Done state

        machine :: rest ->
            let
                result =
                    fewestButtons machine
            in
            case result of
                Err str ->
                    Error str

                Ok presses ->
                    Loop { state | machines = rest, presses = state.presses + presses }


fewestButtons : Machine -> Result String Int
fewestButtons =
    fewestButtonsHelp 0


fewestButtonsHelp : Int -> Machine -> Result String Int
fewestButtonsHelp buttons machine =
    if buttons > List.length machine.buttons then
        Err "tried all configurations - none worked!"

    else if nButtonsWorks buttons machine then
        Ok buttons

    else
        fewestButtonsHelp (buttons + 1) machine


nButtonsWorks : Int -> Machine -> Bool
nButtonsWorks pressedCount machine =
    nButtonsWorksHelp (seq (List.length machine.buttons) pressedCount) machine


nButtonsWorksHelp : List (List Int) -> Machine -> Bool
nButtonsWorksHelp pressed machine =
    case pressed of
        [] ->
            False

        config :: rest ->
            if activates config machine then
                True

            else
                nButtonsWorksHelp rest machine


activates : List Int -> Machine -> Bool
activates pressed machine =
    List.map2 Tuple.pair pressed machine.buttons
        |> List.filter (Tuple.first >> (==) 1)
        |> List.map Tuple.second
        |> List.concat
        |> List.foldl
            (\press lights ->
                lights
                    |> List.indexedMap
                        (\i light ->
                            if i == press then
                                switch light

                            else
                                light
                        )
            )
            machine.lights
        |> List.all ((==) Off)


switch : Light -> Light
switch light =
    case light of
        On ->
            Off

        Off ->
            On


seq : Int -> Int -> List (List Int)
seq n k =
    if k == 0 then
        [ List.repeat n 0 ]

    else if n == 0 then
        []

    else
        (seq (n - 1) (k - 1) |> List.map (\list -> 1 :: list))
            ++ (seq (n - 1) k |> List.map (\list -> 0 :: list))


parser : Parser (List Machine)
parser =
    Shared.linesParser <|
        Parser.succeed Machine
            |= lightsParser
            |= buttonsParser
            |= joltageParser


lightsParser : Parser (List Light)
lightsParser =
    Parser.sequence
        { start = "["
        , separator = ""
        , end = "]"
        , spaces = Parser.chompWhile (always False)
        , item =
            Parser.oneOf
                [ Parser.succeed Off
                    |. Parser.symbol "."
                , Parser.succeed On
                    |. Parser.symbol "#"
                ]
        , trailing = Parser.Optional
        }


buttonsParser : Parser (List Button)
buttonsParser =
    Parser.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = Parser.spaces
        , item =
            Parser.sequence
                { start = "("
                , separator = ","
                , end = ")"
                , spaces = Parser.chompWhile (always False)
                , item = Parser.int
                , trailing = Parser.Optional
                }
        , trailing = Parser.Forbidden
        }


joltageParser : Parser (List Int)
joltageParser =
    Parser.sequence
        { start = "{"
        , separator = ","
        , end = "}"
        , spaces = Parser.chompWhile (always False)
        , item = Parser.int
        , trailing = Parser.Optional
        }


puzzle : Puzzle
puzzle =
    { parts = [ part1 ] }

module Day07 exposing (Field(..), parser, part1, part2, puzzle)

import Array
import Grid exposing (Grid, Position)
import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle, Step(..))


type Field
    = Start
    | Splitter
    | Empty
    | Beam Int


type alias State =
    { grid : Grid Field
    , splits : Int
    , y : Int
    }


part1 : Puzzle.Part
part1 =
    Puzzle.part
        { view = view
        , result = .splits >> String.fromInt
        , parser = parser
        , init = \grid -> { grid = grid, y = 0, splits = 0 }
        , step = advanceBeams
        }


part2 : Puzzle.Part
part2 =
    Puzzle.part
        { view = view
        , result =
            \{ grid } ->
                grid
                    |> Array.get (Grid.height grid - 1)
                    |> Maybe.withDefault Array.empty
                    |> Array.foldl
                        (\field timelines ->
                            case field of
                                Beam int ->
                                    timelines + int

                                _ ->
                                    timelines
                        )
                        0
                    |> String.fromInt
        , parser = parser
        , init = \grid -> { grid = grid, y = 0, splits = 0 }
        , step = advanceBeams
        }


view : State -> List String
view state =
    state.grid
        |> Grid.view
            (\field ->
                case field of
                    Start ->
                        "S"

                    Splitter ->
                        "^"

                    Empty ->
                        "."

                    Beam _ ->
                        "|"
            )
        |> List.indexedMap
            (\i line ->
                if state.y == i then
                    ">" ++ line

                else
                    " " ++ line
            )


advanceBeams : State -> Step State
advanceBeams state =
    if state.y + 1 == Grid.height state.grid then
        Done state

    else
        let
            state2 =
                state.grid
                    |> Array.get state.y
                    |> Maybe.withDefault Array.empty
                    |> Array.foldl
                        (\field acc ->
                            let
                                continueBeam int =
                                    case Grid.get ( acc.x, state.y + 1 ) acc.grid of
                                        Nothing ->
                                            { acc | x = acc.x + 1 }

                                        Just Splitter ->
                                            { acc
                                                | x = acc.x + 1
                                                , splits = acc.splits + 1
                                                , grid =
                                                    acc.grid
                                                        |> addBeam ( acc.x - 1, state.y + 1 ) int
                                                        |> addBeam ( acc.x + 1, state.y + 1 ) int
                                            }

                                        Just _ ->
                                            { acc
                                                | x = acc.x + 1
                                                , grid =
                                                    acc.grid
                                                        |> addBeam ( acc.x, state.y + 1 ) int
                                            }
                            in
                            case field of
                                Beam int ->
                                    continueBeam int

                                Start ->
                                    continueBeam 1

                                _ ->
                                    { acc | x = acc.x + 1 }
                        )
                        { x = 0, splits = 0, grid = state.grid }
        in
        Loop
            { state
                | y = state.y + 1
                , splits = state.splits + state2.splits
                , grid = state2.grid
            }


addBeam : Position -> Int -> Grid Field -> Grid Field
addBeam pos power grid =
    case Grid.get pos grid of
        Just (Beam int) ->
            Grid.set pos (Beam <| power + int) grid

        _ ->
            Grid.set pos (Beam <| power) grid


parser : Parser (Grid Field)
parser =
    Parser.sequence
        { start = ""
        , separator = "\n"
        , end = ""
        , spaces = Parser.chompWhile (always False)
        , item =
            Parser.sequence
                { start = ""
                , separator = ""
                , end = ""
                , spaces = Parser.chompWhile (always False)
                , item =
                    Parser.oneOf
                        [ Parser.succeed Start
                            |. Parser.symbol "S"
                        , Parser.succeed Splitter
                            |. Parser.symbol "^"
                        , Parser.succeed Empty
                            |. Parser.symbol "."
                        ]
                , trailing = Optional
                }
        , trailing = Mandatory
        }
        |> Parser.map Grid.fromLists


puzzle : Puzzle
puzzle =
    { parts = [ part1, part2 ] }

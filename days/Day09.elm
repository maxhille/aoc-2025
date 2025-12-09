module Day09 exposing (parser, part1, puzzle)

import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle, Step(..))
import Shared


type alias Tile =
    ( Int, Int )


type alias State =
    { largest : Int
    , tiles : List Tile
    }


part1 : Puzzle.Part
part1 =
    Puzzle.part
        { view = view
        , result = .largest >> String.fromInt
        , parser = parser
        , init = \tiles -> { tiles = tiles, largest = 0 }
        , step = findLargestRectangle
        }


findLargestRectangle : State -> Step State
findLargestRectangle state =
    case state.tiles of
        [] ->
            Done state

        tile1 :: rest ->
            let
                largest2 =
                    rest
                        |> List.foldl
                            (\tile2 acc ->
                                let
                                    area_ =
                                        area tile1 tile2
                                in
                                if area_ > acc then
                                    area_

                                else
                                    acc
                            )
                            state.largest
            in
            Loop { tiles = rest, largest = largest2 }


area : Tile -> Tile -> Int
area ( x1, y1 ) ( x2, y2 ) =
    abs ((x1 - x2) + 1) * abs ((y1 - y2) + 1)


view : State -> List String
view =
    always []


parser : Parser (List Tile)
parser =
    Shared.linesParser <|
        Parser.succeed Tuple.pair
            |= Parser.int
            |. Parser.symbol ","
            |= Parser.int


puzzle : Puzzle
puzzle =
    { parts = [ part1 ] }

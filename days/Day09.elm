module Day09 exposing (parser, part1, part2, puzzle)

import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle, Step(..))
import Shared


type alias Tile =
    ( Int, Int )


type alias Edge =
    ( Tile, Tile )


type alias State =
    { largest : Int
    , toCheck : List Tile
    , tiles : List Tile
    }


part1 : Puzzle.Part
part1 =
    Puzzle.part
        { view = view
        , result = .largest >> String.fromInt
        , parser = parser
        , init = \tiles -> { toCheck = tiles, largest = 0, tiles = tiles }
        , step = findLargestRectangle False
        }


part2 : Puzzle.Part
part2 =
    Puzzle.part
        { view = view
        , result = .largest >> String.fromInt
        , parser = parser
        , init = \tiles -> { toCheck = tiles, largest = 0, tiles = tiles }
        , step = findLargestRectangle True
        }


findLargestRectangle : Bool -> State -> Step State
findLargestRectangle mustBeInside state =
    case state.toCheck of
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

                                    legal =
                                        if mustBeInside then
                                            isInside state.tiles ( 2, 5 ) ( 11, 7 )

                                        else
                                            True

                                    _ =
                                        Debug.log "legal"
                                            { legal = legal
                                            , tiles = state.tiles
                                            , tile1 = tile1
                                            , tile2 = tile2
                                            }
                                in
                                if area_ > acc && legal then
                                    area_

                                else
                                    acc
                            )
                            state.largest
            in
            Loop { state | toCheck = rest, largest = largest2 }


isInside : List Tile -> Tile -> Tile -> Bool
isInside =
    isInsideHelp { xMin = False, xMax = False, yMin = False, yMax = False }


type alias Around =
    { xMin : Bool, xMax : Bool, yMin : Bool, yMax : Bool }


isInsideHelp : Around -> List Tile -> Tile -> Tile -> Bool
isInsideHelp around tiles ( x1, y1 ) ( x2, y2 ) =
    case tiles of
        [] ->
            around.xMin && around.xMax && around.yMin && around.yMax

        ( xt, yt ) :: rest ->
            let
                around2 =
                    { xMin = around.xMin || (x1 >= xt && x2 >= xt)
                    , xMax = around.xMax || (x1 <= xt && x2 <= xt)
                    , yMin = around.yMin || (y1 >= yt && y2 >= yt)
                    , yMax = around.yMax || (y1 <= yt && y2 <= yt)
                    }

                _ =
                    Debug.log "around"
                        { testTile = ( xt, yt )
                        }
            in
            isInsideHelp around2 rest ( x1, y1 ) ( x2, y2 )


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

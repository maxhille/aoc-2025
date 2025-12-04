module Day04 exposing (Tile(..), parser, part1, puzzle, tilesParser)

import Array
import Grid exposing (Grid, Position)
import Html.Attributes exposing (height, width)
import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle, Step(..))
import Shared


type Tile
    = Empty
    | Paper


type alias State =
    { tiles : Grid Tile
    , accessibles : List Position
    , remaining : List Position
    }


part1 : Puzzle.Part
part1 =
    Puzzle.part
        { view = view
        , result = .accessibles >> List.length >> String.fromInt
        , parser = parser
        , init = \tiles -> { tiles = tiles, accessibles = [], remaining = positionsFromGrid tiles }
        , step = countAccessible
        }


positionsFromGrid : Grid a -> List Position
positionsFromGrid grid =
    let
        width =
            Array.get 0 grid
                |> Maybe.map Array.length
                |> Maybe.withDefault 0

        height =
            Array.length grid
    in
    range ( 0, 0 ) ( width - 1, height - 1 )


range : Position -> Position -> List Position
range ( x1, y1 ) ( x2, y2 ) =
    let
        xs =
            List.range x1 x2

        ys =
            List.range y1 y2
    in
    List.map (\x -> List.map (\y -> ( x, y )) ys) xs |> List.concat


view : State -> List String
view _ =
    []


countAccessible : State -> Step State
countAccessible { tiles, remaining, accessibles } =
    case remaining of
        [] ->
            Done { tiles = tiles, remaining = remaining, accessibles = accessibles }

        pos :: rest ->
            let
                adjacent ( x, y ) =
                    range ( x - 1, y - 1 ) ( x + 1, y + 1 )
                        |> List.filter ((/=) pos)
                        |> List.filterMap (\pos_ -> Grid.get pos_ tiles)

                accessible =
                    if Grid.get pos tiles == Just Paper then
                        adjacent pos
                            |> List.filter ((==) Paper)
                            |> List.length
                            |> (\int -> int < 4)

                    else
                        False

                accessibles2 =
                    if accessible then
                        pos :: accessibles

                    else
                        accessibles
            in
            Loop { tiles = tiles, remaining = rest, accessibles = accessibles2 }


parser : Parser (Grid Tile)
parser =
    Shared.linesParser tilesParser |> Parser.map Grid.fromLists


tilesParser : Parser (List Tile)
tilesParser =
    let
        help revInts =
            Parser.oneOf
                [ Parser.succeed (\_ -> Parser.Loop (Empty :: revInts))
                    |= Parser.symbol "."
                , Parser.succeed (\_ -> Parser.Loop (Paper :: revInts))
                    |= Parser.symbol "@"
                , Parser.succeed ()
                    |> Parser.map (\_ -> Parser.Done (List.reverse revInts))
                ]
    in
    Parser.loop [] help


puzzle : Puzzle
puzzle =
    { parts = [ part1 ] }

module Day04 exposing (Tile(..), parser, part1, part2, puzzle, tilesParser)

import Array
import Grid exposing (Grid, Position)
import Html.Attributes exposing (height, width)
import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle, Step(..))
import Shared


type Tile
    = Empty
    | Paper


type alias State1 =
    { tiles : Grid Tile
    , accessibles : List Position
    , remaining : List Position
    }


type alias State2 =
    { tiles : Grid Tile
    , removed : Int
    }


part1 : Puzzle.Part
part1 =
    Puzzle.part
        { view = .tiles >> viewTiles
        , result = .accessibles >> List.length >> String.fromInt
        , parser = parser
        , init = \tiles -> { tiles = tiles, accessibles = [], remaining = positionsFromGrid tiles }
        , step = findAccessibles
        }


part2 : Puzzle.Part
part2 =
    Puzzle.part
        { view = .tiles >> viewTiles
        , result = .removed >> String.fromInt
        , parser = parser
        , init = \tiles -> { tiles = tiles, removed = 0 }
        , step = removeAccessible
        }


viewTiles : Grid Tile -> List String
viewTiles =
    Grid.view
        (\tile ->
            case tile of
                Empty ->
                    "."

                Paper ->
                    "@"
        )


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
    List.map (\y -> List.map (\x -> ( x, y )) xs) ys |> List.concat


removeAccessible : State2 -> Step State2
removeAccessible { tiles, removed } =
    let
        result =
            Puzzle.execute findAccessibles
                { tiles = tiles
                , accessibles = []
                , remaining = positionsFromGrid tiles
                }
    in
    case result of
        Err str ->
            Error <| "findAccessibles did not work:" ++ str

        Ok { accessibles } ->
            if accessibles == [] then
                Done { tiles = tiles, removed = removed }

            else
                Loop
                    { tiles =
                        tiles
                            |> Grid.positionedMap
                                (\pos tile ->
                                    if List.member pos accessibles then
                                        Empty

                                    else
                                        tile
                                )
                    , removed = removed + List.length accessibles
                    }


findAccessibles : State1 -> Step State1
findAccessibles { tiles, remaining, accessibles } =
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
    { parts = [ part1, part2 ] }

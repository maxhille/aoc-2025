module Day09 exposing (parser, part1, part2, puzzle)

import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle, Step(..))
import Set exposing (Set)
import Shared


type alias Tile =
    ( Int, Int )


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
                                            isInside state.tiles tile1 tile2

                                        else
                                            True
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
isInside tiles ( x1, y1 ) ( x2, y2 ) =
    let
        _ =
            Debug.log "isInside"
                { tile1 = ( x1, y1 )
                , tile2 = ( x2, y2 )
                }

        perimeter =
            let
                start =
                    tiles
                        |> List.reverse
                        |> List.head
                        |> Maybe.withDefault ( 0, 0 )
            in
            tiles
                |> List.foldl
                    (\tile { last, list } ->
                        let
                            interpolated =
                                interpolate last tile
                        in
                        { list = list ++ interpolated
                        , last = tile
                        }
                    )
                    { list = []
                    , last = start
                    }
                |> .list
                |> Set.fromList
    in
    -- (x1,y1)
    raycast (\x -> Set.member ( x, y1 ) perimeter) x1
        && raycast (\y -> Set.member ( x1, y ) perimeter) y1
        -- (x2,y1)
        && raycast (\x -> Set.member ( x, y1 ) perimeter) x2
        && raycast (\y -> Set.member ( x2, y ) perimeter) y1
        -- (x1,y2)
        && raycast (\x -> Set.member ( x, y2 ) perimeter) x1
        && raycast (\y -> Set.member ( x1, y ) perimeter) y2
        -- (x2,y2)
        && raycast (\x -> Set.member ( x, y2 ) perimeter) x2
        && raycast (\y -> Set.member ( x2, y ) perimeter) y2


raycast : (Int -> Bool) -> Int -> Bool
raycast =
    raycastHelp False 0


raycastHelp : Bool -> Int -> (Int -> Bool) -> Int -> Bool
raycastHelp inside val isPerimeter target =
    let
        _ =
            Debug.log "raycast"
                { inside = inside
                , val = val
                , target = target
                }
    in
    if val == target then
        inside

    else
        let
            inside2 =
                if inside then
                    if isPerimeter (val + 2) then
                        not inside

                    else
                        inside

                else if isPerimeter (val + 1) then
                    not inside

                else
                    inside
        in
        raycastHelp inside2 (val + 1) isPerimeter target


interpolate : Tile -> Tile -> List Tile
interpolate ( x1, y1 ) ( x2, y2 ) =
    if x1 == x2 then
        List.range (min y1 y2) (max y1 y2) |> List.map (\y -> ( x1, y ))

    else
        List.range (min x1 x2) (max x1 x2) |> List.map (\x -> ( x, y1 ))


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
    { parts = [ part1, part2 ] }

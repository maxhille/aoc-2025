module Day09 exposing (parser, part1, part2, puzzle)

import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle, Step(..))
import Set exposing (Set)
import Shared


type alias Tile =
    ( Int, Int )


type alias Rectangle =
    ( Tile, Tile )


type alias State1 =
    { rectangles : List Rectangle
    , tiles : List Tile
    }


type alias State2 =
    { rectangles : Maybe (List Rectangle)
    , largestArea : Int
    , perimeter : Set Tile
    , tiles : List Tile
    }


part1 : Puzzle.Part
part1 =
    Puzzle.part
        { view = always []
        , result =
            .rectangles
                >> List.head
                >> Maybe.map area
                >> Maybe.withDefault 0
                >> String.fromInt
        , parser = parser
        , init = \tiles -> { tiles = tiles, rectangles = [] }
        , step = buildAllRectangles
        }


buildAllRectangles : State1 -> Step State1
buildAllRectangles state =
    case state.tiles of
        [] ->
            Done { state | rectangles = state.rectangles |> List.sortBy area |> List.reverse }

        tile1 :: rest ->
            Loop
                { state
                    | tiles = rest
                    , rectangles = state.rectangles ++ List.map (Tuple.pair tile1) rest
                }


part2 : Puzzle.Part
part2 =
    Puzzle.part
        { view = \{ rectangles } -> [ "toCheck: " ++ (rectangles |> Maybe.withDefault [] |> List.length |> String.fromInt) ]
        , result = .largestArea >> String.fromInt
        , parser = parser
        , init =
            \tiles ->
                { rectangles = Nothing
                , largestArea = 0
                , tiles = tiles
                , perimeter =
                    tiles
                        |> List.foldl
                            (\tile { list, last } ->
                                { list = list ++ interpolate tile last
                                , last = tile
                                }
                            )
                            { last = tiles |> List.reverse |> List.head |> Maybe.withDefault ( 0, 0 )
                            , list = []
                            }
                        |> .list
                        |> Set.fromList
                }
        , step = largestInside
        }


largestInside : State2 -> Step State2
largestInside state =
    case state.rectangles of
        Nothing ->
            case Puzzle.execute buildAllRectangles { tiles = state.tiles, rectangles = [] } of
                Ok { rectangles } ->
                    Loop { state | rectangles = Just rectangles }

                Err str ->
                    Error <| "could not execute part1: " ++ str

        Just rectangles ->
            case rectangles of
                [] ->
                    Done state

                rectangle :: rest ->
                    if isInside state.perimeter rectangle then
                        Done { state | largestArea = area rectangle }

                    else
                        Loop { state | rectangles = Just rest }


isInside : Set Tile -> Rectangle -> Bool
isInside perimeter ( ( x1, y1 ), ( x2, y2 ) ) =
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


area : Rectangle -> Int
area ( ( x1, y1 ), ( x2, y2 ) ) =
    (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)


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

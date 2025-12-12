module Day09 exposing (intersect, parser, part1, part2, puzzle)

import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle, Step(..))
import Set exposing (Set)
import Shared


type alias Tile =
    ( Int, Int )


type alias Rectangle =
    ( Tile, Tile )


type alias Edge =
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
    , edges : List Edge
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
                , edges =
                    tiles
                        |> List.foldl
                            (\tile { edges, last } ->
                                { edges = ( tile, last ) :: edges
                                , last = tile
                                }
                            )
                            { last = tiles |> List.reverse |> List.head |> Maybe.withDefault ( 0, 0 )
                            , edges = []
                            }
                        |> .edges
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
                    if isInside state.perimeter rectangle && freeOfIntersections state.edges rectangle then
                        Done { state | largestArea = area rectangle }

                    else
                        Loop { state | rectangles = Just rest }


freeOfIntersections : List Edge -> Rectangle -> Bool
freeOfIntersections edges rectangle =
    edges
        |> List.any (intersect rectangle)
        |> not


intersect : Rectangle -> Edge -> Bool
intersect ( ( xr1, yr1 ), ( xr2, yr2 ) ) ( ( xe1, ye1 ), ( xe2, ye2 ) ) =
    let
        minX =
            min xr1 xr2

        maxX =
            max xr1 xr2

        minY =
            min yr1 yr2

        maxY =
            max yr1 yr2
    in
    if xe1 == xe2 then
        -- vertical edge
        let
            x =
                xe1

            yMinEdge =
                min ye1 ye2

            yMaxEdge =
                max ye1 ye2
        in
        minX < x && x < maxX && (max minY yMinEdge < min maxY yMaxEdge)

    else
        -- horizontal edge
        let
            y =
                ye1

            xMinEdge =
                min xe1 xe2

            xMaxEdge =
                max xe1 xe2
        in
        minY < y && y < maxY && (max minX xMinEdge < min maxX xMaxEdge)


isInside : Set Tile -> Rectangle -> Bool
isInside perimeter ( ( x1, y1 ), ( x2, y2 ) ) =
    [ ( (x1 + x2) // 2, (y1 + y2) // 2 )
    ]
        |> List.all (raycast (\tile -> Set.member tile perimeter))


raycast : (Tile -> Bool) -> Tile -> Bool
raycast isPerimeter ( xt, yt ) =
    raycastHelp False -1 (\x -> isPerimeter ( x, yt )) xt


raycastHelp : Bool -> Int -> (Int -> Bool) -> Int -> Bool
raycastHelp inside val isPerimeter target =
    if val == target then
        inside

    else
        let
            val2 =
                val + 1

            inside2 =
                if isPerimeter val2 then
                    not inside

                else
                    inside
        in
        raycastHelp inside2 val2 isPerimeter target


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

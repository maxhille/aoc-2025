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
        |> Debug.log "edges"
        |> List.any (intersect rectangle)
        |> not
        |> Debug.log "freeOfIntersections"


intersect : Rectangle -> Edge -> Bool
intersect ( ( xr1, yr1 ), ( xr2, yr2 ) ) ( ( xe1, ye1 ), ( xe2, ye2 ) ) =
    if xe1 == xe2 then
        (min xr1 xr2 < xe1 && max xr1 xr2 > xe1)
            && List.any identity
                [ min yr1 yr2 < min ye1 ye2 && max yr1 yr2 > min ye1 ye2 -- partial from above
                , max yr1 yr2 > max ye1 ye2 && min yr1 yr2 > max ye1 ye2 -- partial from below
                , max yr1 yr2 < max ye1 ye2 && min yr1 yr2 > min ye1 ye2 -- full vertical intersection
                , max yr1 yr2 >= max ye1 ye2 && min yr1 yr2 >= min ye1 ye2 -- internal vertical intersection
                ]

    else
        -- ye1 == ye2
        (min yr1 yr2 < ye1 && max yr1 yr2 > ye1)
            && List.any identity
                [ min xr1 xr2 < min xe1 xe2 && max xr1 xr2 > min xe1 xe2 -- partial from right
                , max xr1 xr2 > max xe1 xe2 && min xr1 xr2 > max xe1 xe2 -- partial from left
                , max xr1 xr2 < max xe1 xe2 && min xr1 xr2 > min xe1 xe2 -- full horizontal intersection
                , max xr1 xr2 >= max xe1 xe2 && min xr1 xr2 >= min xe1 xe2 -- full internal intersection
                ]


isInside : Set Tile -> Rectangle -> Bool
isInside perimeter ( ( x1, y1 ), ( x2, y2 ) ) =
    let
        _ =
            Debug.log "rect" <| ( ( x1, y1 ), ( x2, y2 ) )
    in
    [ ( (x1 + x2) // 2, (y1 + y2) // 2 )
    ]
        |> List.all (raycast (\tile -> Set.member tile perimeter))
        |> Debug.log "isInside"


raycast : (Tile -> Bool) -> Tile -> Bool
raycast isPerimeter ( xt, yt ) =
    let
        _ =
            Debug.log "checking"
                { tile = ( xt, yt )
                }
    in
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

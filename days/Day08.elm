module Day08 exposing (Box(..), parser, part1, part2, puzzle)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle, Step(..))
import Shared


type Box
    = Box Int Int Int


type alias Circuit =
    List Box


type alias State =
    { pairs : Maybe Int
    , connections : List ( Box, Box )
    , circuits : List Circuit
    , lastConnection : Maybe ( Box, Box )
    }


part1 : Maybe Int -> Puzzle.Part
part1 pairs =
    Puzzle.part
        { view = view
        , result =
            .circuits
                >> List.map List.length
                >> List.sort
                >> List.reverse
                >> List.take 3
                >> List.product
                >> String.fromInt
        , parser = parser
        , init = init pairs
        , step = pairBoxes
        }


part2 : Puzzle.Part
part2 =
    Puzzle.part
        { view = view
        , result =
            .lastConnection
                >> Maybe.map (\( Box x1 _ _, Box x2 _ _ ) -> x1 * x2)
                >> Maybe.map String.fromInt
                >> Maybe.withDefault "no last connection"
        , parser = parser
        , init = init Nothing
        , step = pairBoxes
        }


init : Maybe Int -> List Box -> State
init pairs boxes =
    { circuits = boxes |> List.map List.singleton
    , pairs = pairs
    , connections = boxes |> connectionsByDistance
    , lastConnection = Nothing
    }


view : State -> List String
view state =
    (case state.pairs of
        Nothing ->
            []

        Just int ->
            [ "pairs: " ++ String.fromInt int ]
    )
        ++ ("circuits: "
                :: (state.circuits
                        |> List.map (List.map viewBox >> String.join " ")
                        |> List.intersperse "\n"
                   )
           )


viewBox : Box -> String
viewBox (Box x y z) =
    [ x, y, z ] |> List.map String.fromInt |> String.join ","


pairBoxes : State -> Step State
pairBoxes state =
    let
        pairsReached =
            state.pairs |> Maybe.map ((==) 0) |> Maybe.withDefault False

        lastCircuit =
            List.length state.circuits == 1
    in
    if pairsReached || lastCircuit then
        Done state

    else
        case state.connections of
            [] ->
                Error "pairs > possible connections"

            ( box1, box2 ) :: rest ->
                Loop
                    { state
                        | pairs = state.pairs |> Maybe.map (\x -> x - 1)
                        , circuits = state.circuits |> mergeCircuits [ box1, box2 ]
                        , connections = rest
                        , lastConnection = Just ( box1, box2 )
                    }


mergeCircuits : List Box -> List Circuit -> List Circuit
mergeCircuits =
    mergeCircuitsHelp []


mergeCircuitsHelp : Circuit -> List Box -> List Circuit -> List Circuit
mergeCircuitsHelp newCircuit toMerge oldCircuits =
    case toMerge of
        [] ->
            newCircuit :: oldCircuits

        box :: rest ->
            let
                ( boxCircuit, oldCircuits2 ) =
                    oldCircuits
                        |> List.partition (List.member box)
                        |> Tuple.mapFirst List.concat
            in
            mergeCircuitsHelp (newCircuit ++ boxCircuit) rest oldCircuits2


connectionsByDistance : List Box -> List ( Box, Box )
connectionsByDistance =
    connectionsByDistanceHelp Dict.empty >> Dict.values


connectionsByDistanceHelp : Dict Int ( Box, Box ) -> List Box -> Dict Int ( Box, Box )
connectionsByDistanceHelp dict boxes =
    case boxes of
        [] ->
            dict

        b1 :: rest ->
            let
                dict2 =
                    rest
                        |> List.foldl (\b2 -> Dict.insert (distance b1 b2) ( b1, b2 )) dict
            in
            connectionsByDistanceHelp dict2 rest


distance : Box -> Box -> Int
distance (Box x1 y1 z1) (Box x2 y2 z2) =
    (x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2


parser : Parser (List Box)
parser =
    Shared.linesParser <|
        Parser.succeed Box
            |= Parser.int
            |. Parser.symbol ","
            |= Parser.int
            |. Parser.symbol ","
            |= Parser.int


puzzle : Puzzle
puzzle =
    { parts = [ part1 (Just 1000), part2 ] }

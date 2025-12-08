module Day08 exposing (parser, part1, puzzle, uniquePairs)

import Dict
import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle, Step(..))


type alias Box =
    ( Int, Int, Int )


type alias Circuit =
    List Box


type alias State =
    { boxes : List Box
    , pairs : Int
    , distances : List ( Box, Box )
    , circuits : List Circuit
    }


part1 : Int -> Puzzle.Part
part1 pairs =
    Puzzle.part
        { view =
            \state ->
                [ "pairs: " ++ String.fromInt state.pairs
                , "circuits: "
                ]
                    ++ (state.circuits
                            |> List.map (List.map viewBox >> String.join " ")
                       )
        , result =
            .circuits
                >> List.map List.length
                >> List.sort
                >> List.reverse
                >> List.take 3
                >> List.product
                >> String.fromInt
        , parser = parser
        , init =
            \boxes ->
                { boxes = boxes
                , circuits = boxes |> List.map List.singleton
                , pairs = pairs
                , distances =
                    boxes
                        |> uniquePairs
                        |> List.foldl (\( box1, box2 ) dict -> Dict.insert ( box1, box2 ) (distance box1 box2) dict) Dict.empty
                        |> Dict.toList
                        |> List.sortBy Tuple.second
                        |> List.map Tuple.first
                }
        , step = pairBoxes
        }


viewBox : Box -> String
viewBox ( x, y, z ) =
    [ x, y, z ] |> List.map String.fromInt |> String.join ","


pairBoxes : State -> Step State
pairBoxes state =
    if state.pairs == 0 then
        Done state

    else
        case state.distances of
            [] ->
                Error "pairs > possible connections"

            ( box1, box2 ) :: rest ->
                Loop
                    { state
                        | pairs = state.pairs - 1
                        , circuits = state.circuits |> mergeCircuits [ box1, box2 ]
                        , distances = rest
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


distance : Box -> Box -> Float
distance ( x1, y1, z1 ) ( x2, y2, z2 ) =
    (x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2 |> toFloat |> sqrt


uniquePairs : List b -> List ( b, b )
uniquePairs =
    uniquePairsHelp []


uniquePairsHelp : List ( b, b ) -> List b -> List ( b, b )
uniquePairsHelp pairs bs =
    case bs of
        [] ->
            pairs

        b1 :: rest ->
            uniquePairsHelp (pairs ++ List.map (\b2 -> ( b1, b2 )) rest) rest


parser : Parser (List ( Int, Int, Int ))
parser =
    Parser.sequence
        { start = ""
        , separator = "\n"
        , end = ""
        , spaces = Parser.chompWhile (always False)
        , item =
            Parser.succeed (\x y z -> ( x, y, z ))
                |= Parser.int
                |. Parser.symbol ","
                |= Parser.int
                |. Parser.symbol ","
                |= Parser.int
        , trailing = Mandatory
        }


puzzle : Puzzle
puzzle =
    { parts = [ part1 1000 ] }

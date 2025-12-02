module Day02 exposing (invalids, parser, part1, puzzle)

import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle, Step(..))


type alias Range =
    ( Int, Int )


type alias State =
    { ranges : List Range
    , invalidIds : List Int
    }


part1 : Puzzle.Part
part1 =
    Puzzle.part
        { view =
            \{ ranges, invalidIds } ->
                [ "invalidIds: " ++ (invalidIds |> List.map String.fromInt |> String.join " ")
                , "ranges to go: " ++ (ranges |> List.length |> String.fromInt)
                ]
        , result = .invalidIds >> List.sum >> String.fromInt
        , parser = parser
        , init = \ranges -> { ranges = ranges, invalidIds = [] }
        , step = countInvalids
        }


countInvalids : State -> Step State
countInvalids { ranges, invalidIds } =
    case ranges of
        range :: rest ->
            Loop { ranges = rest, invalidIds = invalidIds ++ invalids range }

        [] ->
            Done { ranges = ranges, invalidIds = invalidIds }


invalids : Range -> List Int
invalids ( id1, id2 ) =
    invalidsHelp [] id1 id2


invalidsHelp : List Int -> Int -> Int -> List Int
invalidsHelp invalidIds lower upper =
    if lower > upper then
        invalidIds

    else
        invalidsHelp
            (invalidIds
                ++ (if isRepeated lower then
                        [ lower ]

                    else
                        []
                   )
            )
            (lower + 1)
            upper


isRepeated : Int -> Bool
isRepeated int =
    let
        str =
            String.fromInt int
    in
    if (String.length str |> modBy 2) /= 0 then
        False

    else
        let
            len =
                String.length str // 2
        in
        String.left len str == String.right len str


parser : Parser (List Range)
parser =
    Parser.sequence
        { start = ""
        , separator = ","
        , end = ""
        , spaces = Parser.spaces
        , item =
            Parser.succeed Tuple.pair
                |= Parser.int
                |. Parser.symbol "-"
                |= Parser.int
        , trailing = Optional
        }


puzzle : Puzzle
puzzle =
    { parts = [ part1 ] }

module Day02 exposing (invalids, isRepeated1, isRepeated2, parser, part1, part2, puzzle)

import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle, Step(..))


type alias Range =
    ( Int, Int )


type alias State =
    { ranges : List Range
    , invalidIds : List Int
    }


view : State -> List String
view { ranges, invalidIds } =
    [ "invalidIds: " ++ (invalidIds |> List.length |> String.fromInt)
    , "ranges to go: " ++ (ranges |> List.length |> String.fromInt)
    ]


part1 : Puzzle.Part
part1 =
    Puzzle.part
        { view = view
        , result = .invalidIds >> List.sum >> String.fromInt
        , parser = parser
        , init = \ranges -> { ranges = ranges, invalidIds = [] }
        , step = countInvalids isRepeated1
        }


part2 : Puzzle.Part
part2 =
    Puzzle.part
        { view = view
        , result = .invalidIds >> List.sum >> String.fromInt
        , parser = parser
        , init = \ranges -> { ranges = ranges, invalidIds = [] }
        , step = countInvalids isRepeated2
        }


countInvalids : (Int -> Bool) -> State -> Step State
countInvalids isRepeated { ranges, invalidIds } =
    case ranges of
        range :: rest ->
            Loop { ranges = rest, invalidIds = invalidIds ++ invalids isRepeated range }

        [] ->
            Done { ranges = ranges, invalidIds = invalidIds }


invalids : (Int -> Bool) -> Range -> List Int
invalids isRepeated ( id1, id2 ) =
    invalidsHelp isRepeated [] id1 id2


invalidsHelp : (Int -> Bool) -> List Int -> Int -> Int -> List Int
invalidsHelp isRepeated invalidIds lower upper =
    if lower > upper then
        invalidIds

    else
        invalidsHelp
            isRepeated
            (invalidIds
                ++ (if isRepeated lower then
                        [ lower ]

                    else
                        []
                   )
            )
            (lower + 1)
            upper


isRepeated1 : Int -> Bool
isRepeated1 int =
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


isRepeated2 : Int -> Bool
isRepeated2 int =
    isRepeated2Help 1 (String.fromInt int)


isRepeated2Help : Int -> String -> Bool
isRepeated2Help digits id =
    let
        subId =
            String.left digits id
    in
    if digits > String.length id // 2 then
        False

    else if repeats subId id then
        True

    else
        isRepeated2Help (digits + 1) id


repeats : String -> String -> Bool
repeats subId id =
    let
        len =
            String.length subId
    in
    if String.left len id == subId then
        repeats subId (String.dropLeft len id)

    else if id == "" then
        True

    else
        False


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
    { parts = [ part1, part2 ] }

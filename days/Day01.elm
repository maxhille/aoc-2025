module Day01 exposing (Rotation(..), parser, part1, part2, puzzle, rotatePart2)

import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle, Step(..))


type Rotation
    = Left Int
    | Right Int


type alias State =
    { rotations : List Rotation
    , zeros : Int
    , dial : Int
    }


rotatePart1 : Rotation -> Int -> { newZeros : Int, newDial : Int }
rotatePart1 rotation dial =
    let
        newDial =
            case rotation of
                Left clicks ->
                    dial - clicks |> modBy 100

                Right clicks ->
                    dial + clicks |> modBy 100
    in
    { newDial = newDial
    , newZeros =
        if newDial == 0 then
            1

        else
            0
    }


rotatePart2 : Rotation -> Int -> { newZeros : Int, newDial : Int }
rotatePart2 rotation dial =
    let
        newDial =
            case rotation of
                Left clicks ->
                    dial - clicks |> modBy 100

                Right clicks ->
                    dial + clicks |> modBy 100

        fromPosition =
            if newDial == 0 then
                1

            else
                0

        fromTruncation =
            case rotation of
                Left clicks ->
                    clicks // 100

                Right clicks ->
                    clicks // 100

        fromPassing =
            case rotation of
                Left clicks ->
                    if dial /= 0 && dial - (clicks |> modBy 100) < 0 then
                        1

                    else
                        0

                Right clicks ->
                    if dial + (clicks |> modBy 100) > 100 then
                        1

                    else
                        0
    in
    { newDial = newDial
    , newZeros = fromPosition + fromTruncation + fromPassing
    }


part1 : Puzzle.Part
part1 =
    Puzzle.part
        { view = view
        , result = .zeros >> String.fromInt
        , parser = parser
        , init = \rotations -> { rotations = rotations, zeros = 0, dial = 50 }
        , step = password rotatePart1
        }


part2 : Puzzle.Part
part2 =
    Puzzle.part
        { view = view
        , result = .zeros >> String.fromInt
        , parser = parser
        , init = \rotations -> { rotations = rotations, zeros = 0, dial = 50 }
        , step = password rotatePart2
        }


view : State -> List String
view { rotations, zeros, dial } =
    [ "dial: " ++ String.fromInt dial
    , "zeros: " ++ String.fromInt zeros
    , ""
    , "next 3 rotations:"
    ]
        ++ (rotations
                |> List.take 3
                |> List.map
                    (\rotation ->
                        case rotation of
                            Left clicks ->
                                "L" ++ String.fromInt clicks

                            Right clicks ->
                                "R" ++ String.fromInt clicks
                    )
           )


password : (Rotation -> Int -> { newZeros : Int, newDial : Int }) -> State -> Step State
password rotate { rotations, zeros, dial } =
    case rotations of
        rotation :: rest ->
            let
                { newZeros, newDial } =
                    rotate rotation dial
            in
            Loop
                { rotations = rest
                , zeros = zeros + newZeros
                , dial = newDial
                }

        [] ->
            Done { rotations = [], zeros = zeros, dial = dial }


parser : Parser (List Rotation)
parser =
    Parser.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = Parser.spaces
        , item =
            Parser.oneOf
                [ Parser.succeed Left
                    |. Parser.symbol "L"
                    |= Parser.int
                , Parser.succeed Right
                    |. Parser.symbol "R"
                    |= Parser.int
                ]
        , trailing = Optional
        }


puzzle : Puzzle
puzzle =
    { parts = [ part1, part2 ] }

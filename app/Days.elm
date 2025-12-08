module Days exposing (get, puzzles)

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Puzzle exposing (Puzzle)


puzzles : List (Maybe Puzzle)
puzzles =
    [ Just Day01.puzzle
    , Just Day02.puzzle
    , Just Day03.puzzle
    , Just Day04.puzzle
    , Just Day05.puzzle
    , Just Day06.puzzle
    , Just Day07.puzzle
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    ]


get : Int -> Maybe Puzzle
get day =
    puzzles
        |> List.drop (day - 1)
        |> List.head
        |> Maybe.andThen identity

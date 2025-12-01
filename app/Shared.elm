module Shared exposing (get, puzzles)

import Day01
import Puzzle exposing (Part, Puzzle)


puzzles : List (Maybe Puzzle)
puzzles =
    [ Just Day01.puzzle
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
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

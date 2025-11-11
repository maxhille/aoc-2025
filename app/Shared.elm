module Shared exposing (get, puzzles)

import Puzzle exposing (Puzzle)


puzzles : List (Maybe Puzzle)
puzzles =
    [ Nothing
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

module Grid exposing (Grid, Position, fromLists, get)

import Array exposing (Array)


type alias Grid a =
    Array (Array a)


type alias Position =
    ( Int, Int )


get : Position -> Grid a -> Maybe a
get ( x, y ) grid =
    Array.get x grid |> Maybe.andThen (Array.get y)


fromLists : List (List a) -> Grid a
fromLists =
    Array.fromList >> Array.map Array.fromList

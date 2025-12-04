module Grid exposing (Grid, Position, fromLists, get, positionedMap, view)

import Array exposing (Array)


type alias Grid a =
    Array (Array a)


type alias Position =
    ( Int, Int )


view : (a -> String) -> Grid a -> List String
view viewFn =
    let
        rowToString : Array a -> String
        rowToString =
            Array.map viewFn
                >> Array.toList
                >> String.join ""
    in
    Array.map rowToString
        >> Array.toList


get : Position -> Grid a -> Maybe a
get ( x, y ) grid =
    Array.get y grid |> Maybe.andThen (Array.get x)


fromLists : List (List a) -> Grid a
fromLists =
    Array.fromList >> Array.map Array.fromList


positionedMap : (Position -> a -> b) -> Grid a -> Grid b
positionedMap fn =
    Array.indexedMap (\y row -> row |> Array.indexedMap (\x a -> fn ( x, y ) a))

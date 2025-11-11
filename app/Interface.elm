module Interface exposing (add, impl, init, map, wrap)

{-| Hide implementations

credit for these go to jhbrown @ Elm Slack

related presentation slides: <https://docs.google.com/presentation/d/1v16ObhxQdEG2n9ko1tg8LxPL13xgxVSl/edit?slide=id.p34#slide=id.p34>
ellie app from presentation: <https://ellie-app.com/kj4C4wV74wRa1>
related discourse post: <https://discourse.elm-lang.org/t/elm-object-oriented-style/8848>

-}


impl : t -> (raise -> rep -> t)
impl constructor =
    \_ _ -> constructor


wrap : (raise -> rep -> t) -> (raise -> rep -> (t -> q)) -> (raise -> rep -> q)
wrap method pipeline raise rep =
    method raise rep |> pipeline raise rep


add : (rep -> t) -> (raise -> rep -> (t -> q)) -> (raise -> rep -> q)
add method pipeline raise rep =
    method rep |> pipeline raise rep


map : (a -> b) -> (raise -> rep -> a) -> (raise -> rep -> b)
map op pipeline raise rep =
    pipeline raise rep |> op


init : ((rep -> sealed) -> flags -> output) -> ((rep -> sealed) -> rep -> sealed) -> flags -> output
init initRep pipeline flags =
    let
        raise : rep -> sealed
        raise rep =
            pipeline raise rep
    in
    initRep raise flags

port module Worker exposing (Request, Response, main)

import Platform
import Puzzle exposing (Part, part)
import Shared exposing (get)


port fromUi : (Request -> msg) -> Sub msg


port toUi : Response -> Cmd msg


type Msg
    = Init { day : Int, part : Int, input : String }
    | Step Int
    | Error String


type alias Request =
    { type_ : String
    , day : Maybe Int
    , part : Maybe Int
    , input : Maybe String
    , steps : Maybe Int
    }


type alias Response =
    { output : List String
    , result : String
    }


update : Msg -> Maybe Part -> ( Maybe Part, Cmd Msg )
update msg maybePart =
    case msg of
        Init { day, part, input } ->
            case get day |> Maybe.map .parts |> Maybe.withDefault [] |> List.drop (part - 1) |> List.head of
                Just part_ ->
                    let
                        inited =
                            Puzzle.initPart input part_
                    in
                    ( Just inited
                    , inited
                        |> (\p ->
                                { output = Puzzle.viewPart p
                                , result = Puzzle.result p
                                }
                           )
                        |> toUi
                    )

                Nothing ->
                    ( maybePart
                    , toUi
                        { output = []
                        , result = "do not have requested puzzle for day " ++ String.fromInt day
                        }
                    )

        Error str ->
            ( maybePart, toUi { output = [], result = str } )

        Step steps ->
            case maybePart of
                Nothing ->
                    ( maybePart
                    , toUi
                        { output = []
                        , result = "step requested but no puzzle loaded"
                        }
                    )

                Just part ->
                    let
                        stepped =
                            Puzzle.step steps part
                    in
                    ( Just stepped
                    , stepped
                        |> (\part_ ->
                                { output = Puzzle.viewPart part_
                                , result = Puzzle.result part_
                                }
                           )
                        |> toUi
                    )


main : Program () (Maybe Part) Msg
main =
    Platform.worker
        { init = \_ -> ( Nothing, Cmd.none )
        , update = update
        , subscriptions =
            \_ ->
                fromUi
                    (\request ->
                        case request.type_ of
                            "init" ->
                                Maybe.map3 (\day part input -> Init { day = day, part = part, input = input })
                                    request.day
                                    request.part
                                    request.input
                                    |> Maybe.withDefault (Error "worker got 'init' but day and/or input were missing")

                            "step" ->
                                Maybe.map Step
                                    request.steps
                                    |> Maybe.withDefault (Error "worker got 'step' but steps was missing")

                            _ ->
                                Error <| "worker got unknown request type: " ++ request.type_
                    )
        }

port module Main exposing (main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation exposing (Key, load, pushUrl)
import Html exposing (a, button, h1, h2, h3, li, main_, nav, output, p, pre, section, text, textarea, ul)
import Html.Attributes exposing (href, spellcheck, value)
import Html.Events exposing (onClick, onInput)
import Http
import Parser exposing ((|.), (|=))
import Shared exposing (get, puzzles)
import String exposing (fromInt)
import Url exposing (Url)
import Worker exposing (Request, Response)


port fromWorker : (Response -> msg) -> Sub msg


port toWorker : Request -> Cmd msg


type alias Model =
    { input : Input
    , output : List String
    , result : String
    , key : Key
    , day : Int
    , part : Int
    }


type Input
    = Fetching
    | Loaded String
    | Manual String


type Msg
    = OnInput String
    | OnUrlChange Url
    | OnUrlRequest UrlRequest
    | CalculatePart Int
    | ResetPart
    | OnPartResult Response
    | FetchInputResult Int (Result Http.Error String)


fromUrl : Url -> { day : Int, part : Int }
fromUrl url =
    url.fragment
        |> Maybe.withDefault "day1-part1"
        |> Parser.run
            (Parser.succeed (\day part -> { day = day, part = part })
                |. Parser.symbol "day"
                |= Parser.int
                |. Parser.symbol "-part"
                |= Parser.int
            )
        |> Result.withDefault { day = 1, part = 1 }


anchor : Int -> Int -> String
anchor day part =
    "#day" ++ fromInt day ++ "-part" ++ fromInt part


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    let
        { day, part } =
            fromUrl url
    in
    ( { input = Fetching
      , key = key
      , day = day
      , output = []
      , result = "nothing loaded"
      , part = part
      }
    , pushUrl key (anchor day part)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnInput input ->
            ( { model | input = Manual input }
            , toWorker
                { type_ = "init"
                , day = Just model.day
                , part = Just model.part
                , input = Just input
                , steps = Nothing
                }
            )

        FetchInputResult day result ->
            if day == model.day then
                case result of
                    Err _ ->
                        ( { model | input = Manual "" }, Cmd.none )

                    Ok input ->
                        ( { model | input = Loaded input }
                        , toWorker
                            { type_ = "init"
                            , day = Just model.day
                            , part = Just model.part
                            , input = Just input
                            , steps = Nothing
                            }
                        )

            else
                ( model, Cmd.none )

        OnUrlChange url ->
            let
                { day, part } =
                    fromUrl url
            in
            ( { model
                | day = day
                , part = part
                , input = Fetching
                , output = []
              }
            , loadInput day
            )

        OnUrlRequest urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , load url
                    )

        CalculatePart steps ->
            ( model
            , toWorker
                { type_ = "step"
                , steps = Just steps
                , day = Nothing
                , part = Nothing
                , input = Nothing
                }
            )

        ResetPart ->
            case model.input of
                Manual str ->
                    ( model
                    , toWorker
                        { type_ = "init"
                        , steps = Nothing
                        , day = Just model.day
                        , part = Just model.part
                        , input = Just str
                        }
                    )

                Fetching ->
                    ( model, Cmd.none )

                Loaded str ->
                    ( model
                    , toWorker
                        { type_ = "init"
                        , steps = Nothing
                        , day = Just model.day
                        , part = Just model.part
                        , input = Just str
                        }
                    )

        OnPartResult { output, result } ->
            ( { model | output = output, result = result }, Cmd.none )


loadInput : Int -> Cmd Msg
loadInput day =
    Http.get
        { url = "/inputs/" ++ fromInt day ++ ".txt"
        , expect = Http.expectString <| FetchInputResult day
        }


view : Model -> Browser.Document Msg
view model =
    let
        selectedPuzzle =
            puzzles
                |> List.drop (model.day - 1)
                |> List.head
                |> Maybe.andThen identity

        name day =
            "Day " ++ String.fromInt day
    in
    { title = "AoC 2025"
    , body =
        [ h1 [] [ text "🌟 🌟 🌟 Advent of Code 2025 🌟 🌟 🌟" ]
        , main_ []
            [ nav []
                [ h2 [] [ text "Puzzles" ]
                , ul [] <|
                    List.indexedMap
                        (\i puzzle ->
                            li []
                                [ let
                                    day =
                                        i + 1
                                  in
                                  case puzzle of
                                    Just _ ->
                                        a [ href <| "#day" ++ String.fromInt day ++ "-part1" ] [ text <| name day ]

                                    Nothing ->
                                        text <| name day
                                ]
                        )
                        puzzles
                ]
            , section [] <|
                [ h2 []
                    (text (name model.day)
                        :: (get model.day
                                |> Maybe.map .parts
                                |> Maybe.withDefault []
                                |> List.indexedMap
                                    (\i _ ->
                                        let
                                            str =
                                                "Part " ++ fromInt (i + 1)
                                        in
                                        if (i + 1) /= model.part then
                                            a [ href (anchor model.day (i + 1)) ] [ text str ]

                                        else
                                            text str
                                    )
                           )
                        |> List.intersperse (text " - ")
                    )
                , let
                    link =
                        "https://adventofcode.com/2025/day/" ++ String.fromInt model.day
                  in
                  a [ href link ] [ text link ]
                ]
                    ++ (case selectedPuzzle of
                            Just _ ->
                                [ section []
                                    (h3 [] [ text "Input" ]
                                        :: (case model.input of
                                                Manual str ->
                                                    [ textarea [ onInput OnInput, spellcheck False, value str ] [] ]

                                                Loaded _ ->
                                                    [ p [] [ text "Loaded from file", button [ onClick <| OnInput "" ] [ text "manual" ] ] ]

                                                Fetching ->
                                                    [ text "Fetching..." ]
                                           )
                                    )
                                , section [] <|
                                    [ h3 []
                                        [ text "Output"
                                        , button [ onClick ResetPart ] [ text "reset" ]
                                        , button [ onClick (CalculatePart 1) ] [ text "step 1" ]
                                        , button [ onClick (CalculatePart -1) ] [ text "finish" ]
                                        ]
                                    , pre [] <| [ text <| String.join "\n" model.output ]
                                    , pre [] <| [ text model.result ]
                                    ]
                                ]

                            Nothing ->
                                [ p [] [ text <| "there is no puzzle implementation for Day " ++ String.fromInt model.day ]
                                ]
                       )
            ]
        ]
    }


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> fromWorker OnPartResult
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }

module Day11 exposing (Label(..), Rack, parser, part1, puzzle, rackFromList)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser, Trailing(..), oneOf)
import Puzzle exposing (Puzzle, Step(..))
import Shared


type alias Rack =
    Dict String (List Label)


labelToKey : Label -> String
labelToKey label =
    case label of
        Device str ->
            str

        You ->
            "you"

        Out ->
            "out"


rackFromList : List ( Label, List Label ) -> Rack
rackFromList =
    List.map (Tuple.mapFirst labelToKey) >> Dict.fromList


rackGet : Label -> Rack -> List Label
rackGet label rack =
    Dict.get (labelToKey label) rack |> Maybe.withDefault []


type Label
    = Device String
    | You
    | Out


type alias State =
    { rack : Rack
    , paths : List Label
    }


part1 : Puzzle.Part
part1 =
    Puzzle.part
        { view = always []
        , result = .paths >> List.length >> String.fromInt
        , parser = parser
        , init = \rack -> { rack = rack, paths = rackGet You rack }
        , step = step
        }


step : State -> Step State
step state =
    if state.paths |> List.all ((==) Out) then
        Done state

    else
        let
            paths =
                state.paths
                    |> List.foldl
                        (\label acc ->
                            case label of
                                Out ->
                                    acc ++ [ Out ]

                                _ ->
                                    acc ++ rackGet label state.rack
                        )
                        []
        in
        Loop { state | paths = paths }


parser : Parser Rack
parser =
    Parser.succeed Tuple.pair
        |= labelParser
        |= Parser.sequence
            { start = ": "
            , separator = " "
            , end = ""
            , spaces = Parser.chompWhile (always False)
            , item = labelParser
            , trailing = Parser.Optional
            }
        |> Shared.linesParser
        |> Parser.map rackFromList


labelParser : Parser Label
labelParser =
    oneOf
        [ Parser.succeed You
            |. Parser.symbol "you"
        , Parser.succeed Out
            |. Parser.symbol "out"
        , (Parser.getChompedString <|
            Parser.succeed ()
                |. Parser.chompWhile Char.isAlpha
          )
            |> Parser.map Device
        ]


puzzle : Puzzle
puzzle =
    { parts = [ part1 ] }

module Day05 exposing (parser, part1, puzzle)

import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle, Step(..))


type alias Range =
    ( Int, Int )


type alias Database =
    { ranges : List Range
    , ids : List Int
    }


type alias State =
    { db : Database
    , fresh : Int
    }


part1 : Puzzle.Part
part1 =
    Puzzle.part
        { view = always []
        , result = .fresh >> String.fromInt
        , parser = parser
        , init = \db -> { db = db, fresh = 0 }
        , step = countFresh
        }


countFresh : State -> Step State
countFresh { db, fresh } =
    case db.ids of
        [] ->
            Done { db = db, fresh = fresh }

        id :: rest ->
            Loop
                { db = { db | ids = rest }
                , fresh =
                    fresh
                        + (if contained id db.ranges then
                            1

                           else
                            0
                          )
                }


contained : Int -> List Range -> Bool
contained id ranges =
    case ranges of
        [] ->
            False

        ( lower, upper ) :: rest ->
            if lower <= id && upper >= id then
                True

            else
                contained id rest


parser : Parser Database
parser =
    let
        help state =
            Parser.oneOf
                [ Parser.int
                    |> Parser.andThen
                        (\int ->
                            Parser.oneOf
                                [ Parser.succeed
                                    (\upper ->
                                        Parser.Loop
                                            { state | revRanges = ( int, upper ) :: state.revRanges }
                                    )
                                    |. Parser.symbol "-"
                                    |= Parser.int
                                , Parser.succeed
                                    (Parser.Loop
                                        { state
                                            | revIDs = int :: state.revIDs
                                        }
                                    )
                                ]
                        )
                , Parser.succeed (always <| Parser.Loop state)
                    |= Parser.symbol "\n"
                , Parser.succeed ()
                    |. Parser.end
                    |> Parser.map
                        (\_ ->
                            Parser.Done
                                { ranges = List.reverse state.revRanges
                                , ids = List.reverse state.revIDs
                                }
                        )
                ]
    in
    Parser.loop { revRanges = [], revIDs = [] } help


puzzle : Puzzle
puzzle =
    { parts = [ part1 ] }

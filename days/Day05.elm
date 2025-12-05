module Day05 exposing (parser, part1, part2, puzzle)

import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle, Step(..))


type alias Range =
    ( Int, Int )


type alias Database =
    { ranges : List Range
    , ids : List Int
    }


type alias State1 =
    { db : Database
    , fresh : Int
    }


type alias State2 =
    { db : Database
    , merged : List Range
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


part2 : Puzzle.Part
part2 =
    Puzzle.part
        { view = always []
        , result =
            .merged
                >> List.foldl (\( lower, upper ) acc -> acc + (upper - lower + 1)) 0
                >> String.fromInt
        , parser = parser
        , init =
            \db ->
                { db = { db | ranges = List.sortBy Tuple.first db.ranges }
                , merged = []
                }
        , step = mergeRanges
        }


mergeRanges : State2 -> Step State2
mergeRanges { db, merged } =
    case db.ranges of
        [] ->
            Done { db = { db | ranges = [] }, merged = merged }

        [ last ] ->
            Done { db = { db | ranges = [] }, merged = last :: merged }

        ( current1, current2 ) :: ( next1, next2 ) :: rest ->
            if current2 < next1 then
                Loop
                    { db = { db | ranges = ( next1, next2 ) :: rest }
                    , merged = ( current1, current2 ) :: merged
                    }

            else if next2 <= current2 then
                Loop
                    { db = { db | ranges = ( current1, current2 ) :: rest }
                    , merged = merged
                    }

            else
                Loop
                    { db = { db | ranges = ( current1, next2 ) :: rest }
                    , merged = merged
                    }


countFresh : State1 -> Step State1
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
    { parts = [ part1, part2 ] }

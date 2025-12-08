module Day07Test exposing (..)

import Day07 exposing (..)
import Expect
import Grid
import Parser
import Puzzle exposing (compute)
import Test exposing (..)
import Util exposing (sanitize)


example : String
example =
    sanitize <|
        """
    .......S.......
    ...............
    .......^.......
    ...............
    ......^.^......
    ...............
    .....^.^.^.....
    ...............
    ....^.^...^....
    ...............
    ...^.^...^.^...
    ...............
    ..^...^.....^..
    ...............
    .^.^.^.^.^...^.
    ...............
    """


suite : Test
suite =
    describe "Day 7 tests"
        [ test "Part 1 - Example" <|
            \_ ->
                Expect.equal "21" (example |> compute part1)
        , test "Part 2 - Example" <|
            \_ ->
                Expect.equal "40" (example |> compute part2)
        , test "Parser" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            .S.
                            ^.^
                            """
                in
                Expect.equal
                    (Ok <|
                        Grid.fromLists
                            [ [ Empty, Start, Empty ]
                            , [ Splitter, Empty, Splitter ]
                            ]
                    )
                    (Parser.run parser input)
        ]

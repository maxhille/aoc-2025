module Day04Test exposing (..)

import Day04 exposing (..)
import Expect
import Grid
import Parser
import Puzzle exposing (compute)
import Test exposing (..)
import Util exposing (sanitize)


suite : Test
suite =
    describe "Day 4 tests"
        [ test "Part 1 - Example" <|
            \_ ->
                let
                    example =
                        sanitize <|
                            """
                            ..@@.@@@@.
                            @@@.@.@.@@
                            @@@@@.@.@@
                            @.@@@@..@.
                            @@.@@@@.@@
                            .@@@@@@@.@
                            .@.@.@.@@@
                            @.@@@.@@@@
                            .@@@@@@@@.
                            @.@.@@@.@.
                            """
                in
                Expect.equal "13" (example |> compute part1)
        , test "Part 2 - Example" <|
            \_ ->
                let
                    example =
                        sanitize <|
                            """
                            ..@@.@@@@.
                            @@@.@.@.@@
                            @@@@@.@.@@
                            @.@@@@..@.
                            @@.@@@@.@@
                            .@@@@@@@.@
                            .@.@.@.@@@
                            @.@@@.@@@@
                            .@@@@@@@@.
                            @.@.@@@.@.
                            """
                in
                Expect.equal "43" (example |> compute part2)
        , test "parser" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                                ..@
                                .@@
                                """
                in
                Expect.equal
                    (Ok <|
                        Grid.fromLists
                            [ [ Empty, Empty, Paper ]
                            , [ Empty, Paper, Paper ]
                            ]
                    )
                    (Parser.run parser input)
        , test "tilesParser" <|
            \_ ->
                let
                    input =
                        "..@"
                in
                Expect.equal
                    (Ok <|
                        [ Empty, Empty, Paper ]
                    )
                    (Parser.run tilesParser input)
        ]

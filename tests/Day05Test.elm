module Day05Test exposing (..)

import Day05 exposing (..)
import Expect
import Grid
import Parser
import Puzzle exposing (compute)
import Test exposing (..)
import Util exposing (sanitize)


suite : Test
suite =
    describe "Day 5 tests"
        [ test "Part 1 - Example" <|
            \_ ->
                let
                    example =
                        sanitize <|
                            """
                            3-5
                            10-14
                            16-20
                            12-18

                            1
                            5
                            8
                            11
                            17
                            32
                            """
                in
                Expect.equal "3" (example |> compute part1)
        , test "Part 2 - Example" <|
            \_ ->
                let
                    example =
                        sanitize <|
                            """
                            3-5
                            10-14
                            16-20
                            12-18

                            1
                            5
                            8
                            11
                            17
                            32
                            """
                in
                Expect.equal "14" (example |> compute part2)
        , test "parser" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            1-2
                            22-33

                            1
                            34
                            """
                in
                Expect.equal
                    (Ok
                        { ranges = [ ( 1, 2 ), ( 22, 33 ) ]
                        , ids = [ 1, 34 ]
                        }
                    )
                    (Parser.run parser input)
        ]

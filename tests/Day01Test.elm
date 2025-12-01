module Day01Test exposing (..)

import Day01 exposing (..)
import Expect
import Parser
import Puzzle exposing (compute)
import Test exposing (..)
import Util exposing (sanitize)


suite : Test
suite =
    describe "Day 1 tests"
        [ test "Part 1 - Example" <|
            \_ ->
                let
                    example =
                        sanitize <|
                            """
                            L68
                            L30
                            R48
                            L5
                            R60
                            L55
                            L1
                            L99
                            R14
                            L82
                            """
                in
                Expect.equal "3" (example |> compute part1)
        , test "parser" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            R29
                            L0
                            """
                in
                Expect.equal
                    (Ok
                        [ Right 29, Left 0 ]
                    )
                    (Parser.run parser input)
        ]

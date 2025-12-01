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
        , test "Part 2 - Example" <|
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
                Expect.equal "6" (example |> compute part2)
        , describe
            "Part 2 - Rotations"
            [ test "Ending in 0" <|
                \_ ->
                    Expect.equal { newDial = 0, newZeros = 1 } (rotatePart2 (Left 12) 12)
            , test "Ending in 0 with extra round" <|
                \_ ->
                    Expect.equal { newDial = 0, newZeros = 2 } (rotatePart2 (Left 112) 12)
            , test "Passing 0 left" <|
                \_ ->
                    Expect.equal { newDial = 90, newZeros = 1 } (rotatePart2 (Left 20) 10)
            , test "Passing 0 right" <|
                \_ ->
                    Expect.equal { newDial = 20, newZeros = 1 } (rotatePart2 (Right 40) 80)
            , test "Not passing 0 left" <|
                \_ ->
                    Expect.equal { newDial = 95, newZeros = 0 } (rotatePart2 (Left 5) 0)
            ]
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

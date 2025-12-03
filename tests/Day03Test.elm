module Day03Test exposing (..)

import Day03 exposing (..)
import Expect
import Parser
import Puzzle exposing (compute)
import Test exposing (..)
import Util exposing (sanitize)


suite : Test
suite =
    describe "Day 3 tests"
        [ test "Part 1 - Example" <|
            \_ ->
                let
                    example =
                        sanitize <|
                            """
                            987654321111111
                            811111111111119
                            234234234234278
                            818181911112111
                            """
                in
                Expect.equal "357" (example |> compute part1)
        , test "parser" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            123
                            456
                            """
                in
                Expect.equal
                    (Ok <|
                        [ [ 1, 2, 3 ]
                        , [ 4, 5, 6 ]
                        ]
                    )
                    (Parser.run parser input)
        , test "bankParser" <|
            \_ ->
                let
                    input =
                        "12345"
                in
                Expect.equal
                    (Ok <|
                        [ 1, 2, 3, 4, 5 ]
                    )
                    (Parser.run bankParser input)
        ]

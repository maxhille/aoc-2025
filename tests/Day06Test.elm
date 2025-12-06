module Day06Test exposing (..)

import Day06 exposing (..)
import Expect
import Parser
import Puzzle exposing (compute)
import Test exposing (..)
import Util exposing (sanitize)


suite : Test
suite =
    describe "Day 6 tests"
        [ test "Part 1 - Example" <|
            \_ ->
                let
                    example =
                        sanitize <|
                            """
                            123 328  51 64 
                             45 64  387 23 
                              6 98  215 314
                            *   +   *   +
                            """
                in
                Expect.equal "4277556" (example |> compute part1)
        , only <|
            test "parser" <|
                \_ ->
                    let
                        input =
                            sanitize <|
                                """
                            123 328  51 64
                             45 64  387 23
                              6 98  215 314
                            *   +   *   +
                            """
                    in
                    Expect.equal
                        (Ok
                            { numbers =
                                [ [ 123, 328, 51, 64 ]
                                , [ 45, 64, 387, 23 ]
                                , [ 6, 98, 215, 314 ]
                                ]
                            , ops = [ Mul, Add, Mul, Add ]
                            }
                        )
                        (Parser.run parser input)
        ]

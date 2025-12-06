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
        , test "Part 2 - Example" <|
            \_ ->
                let
                    example =
                        """123 328  51 64  
 45 64  387 23 
  6 98  215 314
*   +   *   +  
"""
                in
                Expect.equal "3263827" (example |> compute part2)
        , test "Part 1 - Parser" <|
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
                    (Parser.run parser1 input)
        , test "Part 2 - Parser" <|
            \_ ->
                let
                    input =
                        """123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  
"""
                in
                Expect.equal
                    (Ok
                        [ { numbers = [ 4, 431, 623 ], op = Add }
                        , { numbers = [ 175, 581, 32 ], op = Mul }
                        , { numbers = [ 8, 248, 369 ], op = Add }
                        , { numbers = [ 356, 24, 1 ], op = Mul }
                        ]
                    )
                    (Parser.run parser2 input)
        , test "Part 2 - ProblemParser" <|
            \_ ->
                let
                    input =
                        """4
                            431
                            623+

                            175
                            581
                            32*
                            """
                in
                Expect.equal
                    (Ok
                        [ { numbers = [ 4, 431, 623 ], op = Add }
                        , { numbers = [ 175, 581, 32 ], op = Mul }
                        ]
                    )
                    (Parser.run problemParser2 input)
        ]

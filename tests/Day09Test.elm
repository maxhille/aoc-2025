module Day09Test exposing (..)

import Day09 exposing (..)
import Expect
import Parser
import Puzzle exposing (compute)
import Test exposing (..)
import Util exposing (sanitize)


example : String
example =
    sanitize <|
        """
        7,1
        11,1
        11,7
        9,7
        9,5
        2,5
        2,3
        7,3
        """


suite : Test
suite =
    describe "Day 9 tests"
        [ test "Part 1 - Example" <|
            \_ ->
                Expect.equal "50" (example |> compute part1)
        , test "Part 2 - Example" <|
            \_ ->
                Expect.equal "24" (example |> compute part2)
        , test "intersect" <|
            \_ ->
                Expect.equal True (intersect ( ( 11, 1 ), ( 2, 5 ) ) ( ( 2, 3 ), ( 7, 3 ) ))
        , test "Parser" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            162,817
                            57,618
                            """
                in
                Expect.equal
                    (Ok <|
                        [ ( 162, 817 )
                        , ( 57, 618 )
                        ]
                    )
                    (Parser.run parser input)
        ]

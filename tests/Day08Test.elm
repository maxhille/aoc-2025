module Day08Test exposing (..)

import Day08 exposing (..)
import Expect
import Parser
import Puzzle exposing (compute)
import Test exposing (..)
import Util exposing (sanitize)


example : String
example =
    sanitize <|
        """
        162,817,812
        57,618,57
        906,360,560
        592,479,940
        352,342,300
        466,668,158
        542,29,236
        431,825,988
        739,650,466
        52,470,668
        216,146,977
        819,987,18
        117,168,530
        805,96,715
        346,949,466
        970,615,88
        941,993,340
        862,61,35
        984,92,344
        425,690,689
        """


suite : Test
suite =
    describe "Day 8 tests"
        [ test "Part 1 - Example" <|
            \_ ->
                Expect.equal "40" (example |> compute (part1 10))
        , test "uniquePairs" <|
            \_ ->
                Expect.equal [ ( 1, 2 ), ( 1, 3 ), ( 2, 3 ) ] ([ 1, 2, 3 ] |> uniquePairs)
        , test "Parser" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            162,817,812
                            57,618,57
                            """
                in
                Expect.equal
                    (Ok <|
                        [ ( 162, 817, 812 )
                        , ( 57, 618, 57 )
                        ]
                    )
                    (Parser.run parser input)
        ]

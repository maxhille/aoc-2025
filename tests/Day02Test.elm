module Day02Test exposing (..)

import Day02 exposing (..)
import Expect
import Parser
import Puzzle exposing (compute)
import Test exposing (..)
import Util exposing (sanitize)


suite : Test
suite =
    describe "Day 2 tests"
        [ test "Part 1 - Example" <|
            \_ ->
                let
                    example =
                        sanitize <|
                            """
                            11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
                            1698522-1698528,446443-446449,38593856-38593862,565653-565659,
                            824824821-824824827,2121212118-2121212124
                            """
                in
                Expect.equal "1227775554" (example |> compute part1)
        , test "Part 2 - Example" <|
            \_ ->
                let
                    example =
                        sanitize <|
                            """
                            11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
                            1698522-1698528,446443-446449,38593856-38593862,565653-565659,
                            824824821-824824827,2121212118-2121212124
                            """
                in
                Expect.equal "4174379265" (example |> compute part2)
        , test "parser" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            11-22,95-115,998-1012,
                            1698522-1698528,446443-446449
                            """
                in
                Expect.equal
                    (Ok
                        [ ( 11, 22 )
                        , ( 95, 115 )
                        , ( 998, 1012 )
                        , ( 1698522, 1698528 )
                        , ( 446443, 446449 )
                        ]
                    )
                    (Parser.run parser input)
        , describe
            "Part 1 - Invalids"
            [ test "11-22" <|
                \_ ->
                    Expect.equal [ 11, 22 ] (invalids isRepeated1 ( 11, 22 ))
            ]
        , describe
            "Part 2 - Invalids"
            [ test "11-22" <|
                \_ ->
                    Expect.equal [ 11, 22 ] (invalids isRepeated2 ( 11, 22 ))
            ]
        , describe
            "Part 2 - isRepeated2"
            [ test "12341234" <|
                \_ ->
                    Expect.equal True (isRepeated2 12341234)
            , test "1212121212" <|
                \_ ->
                    Expect.equal True (isRepeated2 1212121212)
            , test "12" <|
                \_ ->
                    Expect.equal False (isRepeated2 12)
            ]
        ]

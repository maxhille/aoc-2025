module Day11Test exposing (..)

import Day11 exposing (..)
import Expect
import Parser
import Puzzle exposing (compute)
import Test exposing (..)
import Util exposing (sanitize)


example : String
example =
    sanitize <|
        """
        aaa: you hhh
        you: bbb ccc
        bbb: ddd eee
        ccc: ddd eee fff
        ddd: ggg
        eee: out
        fff: out
        ggg: out
        hhh: ccc fff iii
        iii: out
        """


suite : Test
suite =
    describe "Day 11 tests"
        [ test "Part 1 - Example" <|
            \_ ->
                Expect.equal "5" (example |> compute part1)
        , test "Parser" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            aaa: you hhh
                            you: out ccc
                            """
                in
                Expect.equal
                    (Ok <|
                        rackFromList
                            [ ( Device "aaa", [ You, Device "hhh" ] )
                            , ( You, [ Out, Device "ccc" ] )
                            ]
                    )
                    (Parser.run parser input)
        ]

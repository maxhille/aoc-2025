module Day10Test exposing (..)

import Day10 exposing (..)
import Expect
import Parser
import Puzzle exposing (compute)
import Test exposing (..)
import Util exposing (sanitize)


example : String
example =
    sanitize <|
        """
        [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
        [...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
        [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
        """


suite : Test
suite =
    describe "Day 10 tests"
        [ test "Part 1 - Example" <|
            \_ ->
                Expect.equal "7" (example |> compute part1)
        , test "fewestButtons" <|
            \_ ->
                let
                    machine =
                        { lights = [ Off, On, On, Off ]
                        , buttons = [ [ 3 ], [ 1, 3 ], [ 2 ], [ 2, 3 ], [ 0, 2 ], [ 0, 1 ] ]
                        , joltages = [ 3, 5, 4, 7 ]
                        }
                in
                Expect.equal (Ok 2) (fewestButtons machine)
        , test "seq" <|
            \_ ->
                Expect.equal [ [ 1, 0 ], [ 0, 1 ] ] (seq 2 1)
        , test "Parser" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            [.#] (3) (1,3) {3,5}
                            [..] (0,2) (2,3) {7,2}
                            """
                in
                Expect.equal
                    (Ok <|
                        [ { lights = [ Off, On ]
                          , buttons =
                                [ [ 3 ]
                                , [ 1, 3 ]
                                ]
                          , joltages = [ 3, 5 ]
                          }
                        , { lights = [ Off, Off ]
                          , buttons =
                                [ [ 0, 2 ]
                                , [ 2, 3 ]
                                ]
                          , joltages = [ 7, 2 ]
                          }
                        ]
                    )
                    (Parser.run parser input)
        ]

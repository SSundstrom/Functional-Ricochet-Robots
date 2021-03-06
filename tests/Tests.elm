module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Main exposing (prop_robot, prop_win)
import Board exposing (prop_emptyBoard)


suite : Test
suite = describe "all the tests"
    [ prop_robot,
      prop_emptyBoard,
      prop_win
    ]

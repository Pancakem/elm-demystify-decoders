port module Main exposing (..)

import Exercise01.Tests
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    run emit Exercise01.Tests.all


port emit : ( String, Value ) -> Cmd msg

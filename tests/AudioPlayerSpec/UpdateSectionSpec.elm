module AudioPlayerSpec.UpdateSectionSpec exposing (..)

import AudioPlayer
    exposing
        ( Section(..)
        , SectionMsg(..)
        , SectionValidationResult(..)
        , Time(..)
        , unwrapTime
        , updateSection
        )
import Expect
import Fuzz
import Random
import Shrink
import Test exposing (..)
import TestUtil.Generator exposing (minDuration, timeGenerator, timeShrinker)


parameterGenerator : Random.Generator ( Time, Time, Time )
parameterGenerator =
    Random.map
        (\duration ->
            let
                v =
                    unwrapTime duration - (unwrapTime minDuration - 0.001)

                newV =
                    v - 0.0001
            in
            ( duration, Time v, Time newV )
        )
        timeGenerator


parameterFuzzer : Fuzz.Fuzzer ( Time, Time, Time )
parameterFuzzer =
    Fuzz.custom parameterGenerator (Shrink.tuple3 ( timeShrinker, timeShrinker, timeShrinker ))


suite : Test
suite =
    describe "updateSection"
        [ describe "SetStartPoint handler" setStartPointHandlerSpec
        ]


setStartPointHandlerSpec : List Test
setStartPointHandlerSpec =
    let
        doTest : Time -> Time -> Section -> SectionValidationResult -> Expect.Expectation
        doTest duration newV source expected =
            let
                actual =
                    updateSection (SetStartPoint (unwrapTime newV)) duration source
            in
            Expect.equal actual ( expected, Cmd.none )
    in
    [ fuzz parameterFuzzer "overwrites startpoint when it is set only startpoint" <|
        \( duration, v, newV ) ->
            doTest duration newV (SectionStartOnly v) (UpdateSectionOk (SectionStartOnly newV))
    , fuzz parameterFuzzer "makes section range when it is set only endpoint" <|
        \( duration, v, newV ) ->
            doTest duration newV (SectionRange { start = v, end = duration }) (UpdateSectionOk (SectionRange { start = newV, end = duration }))
    , fuzz parameterFuzzer "turns it to section range when it is set only endpoint" <|
        \( duration, _, newV ) ->
            doTest duration newV (SectionEndOnly duration) (UpdateSectionOk (SectionRange { start = newV, end = duration }))
    ]

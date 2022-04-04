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
import Test exposing (..)
import TestUtil.Fuzzer as F


parameter : Fuzz.Fuzzer ( Time, Time, Time )
parameter =
    Fuzz.map
        (\duration ->
            let
                rawDuration =
                    unwrapTime duration

                currentValue =
                    rawDuration / 2

                newValue =
                    currentValue + 0.01
            in
            ( duration, Time currentValue, Time newValue )
        )
        F.duration


suite : Test
suite =
    describe "updateSection"
        [ describe "SetStartPoint handler handles without error and" setStartPointHandlerCorrectlySpec
        ]


doTest : Time -> Time -> Section -> SectionValidationResult -> Expect.Expectation
doTest duration newValue section expected =
    let
        actual =
            updateSection (SetStartPoint (unwrapTime newValue)) duration section
    in
    Expect.equal actual ( expected, Cmd.none )


setStartPointHandlerCorrectlySpec : List Test
setStartPointHandlerCorrectlySpec =
    [ fuzz parameter "overwrites startpoint when it is set only startpoint" <|
        \( duration, currentValue, newValue ) ->
            doTest duration newValue (SectionStartOnly currentValue) (UpdateSectionOk (SectionStartOnly newValue))
    , fuzz parameter "updates section range startpoint when it is set section range already" <|
        \( duration, currentValue, newValue ) ->
            doTest duration newValue (SectionRange { start = currentValue, end = duration }) (UpdateSectionOk (SectionRange { start = newValue, end = duration }))
    , fuzz parameter "turns it to section range when it is set only endpoint" <|
        \( duration, _, newValue ) ->
            doTest duration newValue (SectionEndOnly duration) (UpdateSectionOk (SectionRange { start = newValue, end = duration }))
    ]

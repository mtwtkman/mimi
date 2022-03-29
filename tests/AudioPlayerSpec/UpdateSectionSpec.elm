module AudioPlayerSpec.UpdateSectionSpec exposing (..)

import AudioPlayer exposing (Section(..), SectionMsg(..), updateSection)
import Expect
import Fuzz
import Random
import Shrink
import Test exposing (..)
import TestUtil.Generator exposing (durationGenerator, minDuration)


parameterGenerator : Random.Generator ( Float, Float, Float )
parameterGenerator =
    Random.map
        (\duration ->
            let
                v =
                    duration - (minDuration - 0.001)

                newV =
                    v - 0.0001
            in
            ( duration, v, newV )
        )
        durationGenerator


parameterFuzzer : Fuzz.Fuzzer ( Float, Float, Float )
parameterFuzzer =
    Fuzz.custom parameterGenerator (Shrink.tuple3 ( Shrink.float, Shrink.float, Shrink.float ))


suite : Test
suite =
    describe "updateSection"
        [ describe "SetStartPoint handler" setStartPointHandlerSpec
        ]


setStartPointHandlerSpec : List Test
setStartPointHandlerSpec =
    let
        doTest : Float -> Float -> Section -> Section -> Expect.Expectation
        doTest duration newV source expected =
            let
                actual =
                    updateSection (SetStartPoint newV) duration source
            in
            Expect.equal actual ( Just expected, Cmd.none )
    in
    [ fuzz parameterFuzzer "overwrites startpoint when it is set only startpoint" <|
        \( duration, v, newV ) ->
            doTest duration newV (SectionStartOnly v) (SectionStartOnly newV)
    , fuzz parameterFuzzer "makes section range when it is set only endpoint" <|
        \( duration, v, newV ) ->
            doTest duration newV (SectionRange { start = v, end = duration }) (SectionRange { start = newV, end = duration })
    , fuzz parameterFuzzer "turns it to section range when it is set only endpoint" <|
        \( duration, _, newV ) ->
            doTest duration newV (SectionEndOnly duration) (SectionRange { start = newV, end = duration })
    ]

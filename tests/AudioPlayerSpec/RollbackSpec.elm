module AudioPlayerSpec.RollbackSpec exposing (..)

import AudioPlayer
    exposing
        ( Model
        , Name(..)
        , Time(..)
        , Url(..)
        , defaultStartPoint
        , rollback
        , unwrapDuration
        )
import Expect
import Test exposing (..)
import TestUtil.Fuzzer exposing (durationFixedModel)
import TestUtil.Transform exposing (setEndOnly, setSectionRange, setStartOnly, unsetSection)


suite : Test
suite =
    describe "rollback"
        (rollbackToDefaultStartPointSpec
            ++ rollbackToSetStartPointSpec
        )


doTest : Time -> Model -> Expect.Expectation
doTest expected m =
    Expect.equal (rollback m).currentTime expected


rollbackToDefaultStartPointSpec : List Test
rollbackToDefaultStartPointSpec =
    let
        t =
            doTest defaultStartPoint
    in
    [ fuzz durationFixedModel "section is not set" <|
        \model ->
            let
                noSectionModel =
                    unsetSection model
            in
            t noSectionModel
    , fuzz durationFixedModel "section is set as endonly" <|
        \model ->
            let
                dur =
                    unwrapDuration model

                endOnlyModel =
                    setEndOnly (Time (dur - 0.001)) model
            in
            t endOnlyModel
    ]


rollbackToSetStartPointSpec : List Test
rollbackToSetStartPointSpec =
    [ fuzz durationFixedModel "section is set as startonly" <|
        \model ->
            let
                dur =
                    unwrapDuration model

                s =
                    Time (dur - 0.001)

                startOnlyModel =
                    setStartOnly s model
            in
            doTest s startOnlyModel
    , fuzz durationFixedModel "section range is set" <|
        \model ->
            let
                dur =
                    unwrapDuration model

                s =
                    dur - 0.003

                e =
                    s + 0.001

                ts =
                    Time s

                sectionRangeModel =
                    setSectionRange ts (Time e) model
            in
            doTest ts sectionRangeModel
    ]

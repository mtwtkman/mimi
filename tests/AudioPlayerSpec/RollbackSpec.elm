module AudioPlayerSpec.RollbackSpec exposing (..)

import AudioPlayer
    exposing
        ( Model
        , Name(..)
        , Time(..)
        , Url(..)
        , defaultStartPoint
        , initModel
        , rollback
        )
import Expect
import Test exposing (..)
import TestUtil.Transform exposing (fixDuration, setEndOnly, setSectionRange, setStartOnly, unsetSection)


model : Time -> Model
model dur =
    initModel (Name "a") (Url "b") |> fixDuration dur


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
    [ test "section is not set" <|
        \_ ->
            let
                noSectionModel =
                    unsetSection <| model (Time 10.0)
            in
            t noSectionModel
    , test "section is set as endonly" <|
        \_ ->
            let
                dur =
                    10.0

                m =
                    model (Time dur)

                endOnlyModel =
                    setEndOnly (Time (dur - 1.0)) m
            in
            t endOnlyModel
    ]


rollbackToSetStartPointSpec : List Test
rollbackToSetStartPointSpec =
    [ test "section is set as startonly" <|
        \_ ->
            let
                dur =
                    10.1

                s =
                    Time (dur - 3.4)

                m =
                    model (Time dur)

                startOnlyModel =
                    setStartOnly s m
            in
            doTest s startOnlyModel
    , test "section range is set" <|
        \_ ->
            let
                dur =
                    11.2

                m =
                    model (Time dur)

                s =
                    dur - 4.3

                e =
                    s + 1.1

                ts =
                    Time s

                sectionRangeModel =
                    setSectionRange ts (Time e) m
            in
            doTest ts sectionRangeModel
    ]

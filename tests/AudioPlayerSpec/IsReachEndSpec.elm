module AudioPlayerSpec.IsReachEndSpec exposing (..)

import AudioPlayer exposing (Model, Section(..), Time(..), isReachedEnd, unwrapDuration, unwrapTime)
import Expect
import Fuzz
import Test exposing (..)
import TestUtil.Fuzzer exposing (durationFixedModel)
import TestUtil.Transform exposing (setCurrentTime, setEndOnly, setSectionRange, setStartOnly, unsetSection)


suite : Test
suite =
    describe "isReachEnd"
        [ describe "returns true" currentTimeReachedToEndpointCase
        , describe "returns false" currentTimeIsBeforeEndpointCase
        ]


durationFixedModelExposedDuration : Fuzz.Fuzzer ( Model, Float )
durationFixedModelExposedDuration =
    Fuzz.map
        (\m -> ( m, unwrapDuration m ))
        durationFixedModel


doTest : Bool -> Model -> Expect.Expectation
doTest expectedBool model =
    let
        ( testFunc, boolString ) =
            if expectedBool then
                ( Expect.true, "true" )

            else
                ( Expect.false, "false" )
    in
    testFunc
        ("Expected " ++ boolString)
        (isReachedEnd model)


currentTimeReachedToEndpointCase : List Test
currentTimeReachedToEndpointCase =
    let
        expectTrue =
            doTest True
    in
    [ fuzz durationFixedModelExposedDuration "when EndPointOnly is set" <|
        \( model, duration ) ->
            let
                t =
                    Time (duration - 0.001)

                endpointSetModel =
                    setEndOnly t >> setCurrentTime t <| model
            in
            expectTrue endpointSetModel
    , fuzz durationFixedModelExposedDuration "when SectionRange is set" <|
        \( model, duration ) ->
            let
                s =
                    duration - 0.003

                e =
                    Time (s + 0.001)

                sectionRangeSetModel =
                    setSectionRange (Time s) e >> setCurrentTime e <| model
            in
            expectTrue sectionRangeSetModel
    , fuzz durationFixedModelExposedDuration "when no section is set" <|
        \( model, duration ) ->
            let
                noSectionModel =
                    unsetSection >> setCurrentTime (Time duration) <| model
            in
            expectTrue noSectionModel
    , fuzz durationFixedModelExposedDuration "when StartPointOnly is set (but should be ignored)" <|
        \( model, duration ) ->
            let
                s =
                    Time (duration - 0.001)

                startpointSetModel =
                    setStartOnly s >> setCurrentTime (Time duration) <| model
            in
            expectTrue startpointSetModel
    ]


currentTimeIsBeforeEndpointCase : List Test
currentTimeIsBeforeEndpointCase =
    let
        setBeforeEndpoint : Time -> Model -> Model
        setBeforeEndpoint ep m =
            { m | currentTime = Time (unwrapTime ep - 0.0001) }

        expectFalse : Model -> Expect.Expectation
        expectFalse =
            doTest False
    in
    [ fuzz durationFixedModelExposedDuration "when EndPointOnly is set" <|
        \( model, duration ) ->
            let
                e =
                    duration - 0.001

                te =
                    Time e

                endpointSetModel =
                    setEndOnly te >> setBeforeEndpoint te <| model
            in
            expectFalse endpointSetModel
    , fuzz durationFixedModelExposedDuration "when SectionRange is set" <|
        \( model, duration ) ->
            let
                s =
                    duration - 0.005

                e =
                    s + 0.001

                te =
                    Time e

                sectionRangeSetModel =
                    setSectionRange (Time s) te >> setBeforeEndpoint te <| model
            in
            expectFalse sectionRangeSetModel
    , fuzz durationFixedModelExposedDuration "when no section is set" <|
        \( model, duration ) ->
            let
                noSectionModel =
                    unsetSection >> setBeforeEndpoint (Time duration) <| model
            in
            expectFalse noSectionModel
    , fuzz durationFixedModelExposedDuration "when StartPointOnly is set (but should be ignored)" <|
        \( model, duration ) ->
            let
                s =
                    Time (duration - 0.1)

                startpointSetModel =
                    setStartOnly s >> setBeforeEndpoint (Time duration) <| model
            in
            expectFalse startpointSetModel
    ]

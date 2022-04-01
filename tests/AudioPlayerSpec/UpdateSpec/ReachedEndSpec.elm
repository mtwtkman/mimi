module AudioPlayerSpec.UpdateSpec.ReachedEndSpec exposing (..)

import AudioPlayer exposing
    ( Model
    , Msg(..)
    , Section(..)
    , Source
    , Time
    , defaultStartPoint
    , initModel
    , update
    , Time(..)
    , Name(..)
    , Url(..)
    , unwrapTime
    )
import Expect
import Fuzz
import Ports exposing (seek)
import Random
import Shrink
import Test exposing (..)
import TestUtil.Generator exposing (boolGenerator, timeGenerator, maxDuration, timeShrinker)


doTest : Model -> Time -> Model -> Expect.Expectation
doTest model startPoint expected =
    let
        cmd =
            seek (unwrapTime startPoint)
    in
    Expect.equal (update ReachedEnd model) ( expected, cmd )


getDuration : Model -> Time
getDuration x =
    Maybe.withDefault maxDuration x.source.duration


buildModel : Bool -> Time -> Model
buildModel loop duration =
    let
        name =
            "name"

        url =
            "url"

        source =
            Source (Name name) (Url url) (Just duration)

        m =
            initModel name url
    in
    { m | source = source, loop = loop }


modelFuzzer : Fuzz.Fuzzer Model
modelFuzzer =
    Fuzz.custom
        (Random.map2 buildModel boolGenerator timeGenerator)
        ( \model ->
            Shrink.map buildModel (Shrink.bool model.loop)
                |> Shrink.andMap (timeShrinker (Maybe.withDefault maxDuration model.source.duration))
        )


noLoopModel : Time -> Model
noLoopModel =
    buildModel False


loopEnabledModel : Time -> Model
loopEnabledModel =
    buildModel True


expectRollbackToDefaultStartPoint : Model -> Model -> Expect.Expectation
expectRollbackToDefaultStartPoint model expected =
    doTest model defaultStartPoint expected


reachedEndSpec : Test
reachedEndSpec =
    describe "ReachedEnd msg"
        [ describe "rollbacks to default startpoint"
            [ fuzz modelFuzzer "when section setting is none" <|
                \model ->
                    let
                        noSectionModel =
                            { model | section = Nothing }

                        expected =
                            { noSectionModel | currentTime = defaultStartPoint }
                    in
                    expectRollbackToDefaultStartPoint noSectionModel expected
            , fuzz modelFuzzer "when section setting is only endpoint" <|
                \model ->
                    let
                        sectionEndOnlyModel =
                            { model | section = Just (SectionEndOnly (Time (unwrapTime defaultStartPoint + 0.1))) }

                        expected =
                            { sectionEndOnlyModel | currentTime = defaultStartPoint }
                    in
                    expectRollbackToDefaultStartPoint sectionEndOnlyModel expected
            ]
        , describe "rollbacks to user defined startpoint"
            [ fuzz modelFuzzer "when section setting is only startpoint" <|
                \model ->
                    let
                        startPoint =
                            Time (unwrapTime defaultStartPoint + 0.1)

                        sectionStartOnlyModel =
                            { model | section = Just (SectionStartOnly startPoint) }

                        expected =
                            { sectionStartOnlyModel | currentTime = startPoint}
                    in
                    doTest sectionStartOnlyModel startPoint expected
            , fuzz modelFuzzer "when section range is set" <|
                \model ->
                    let
                        duration =
                            getDuration model

                        startPoint =
                            Time (unwrapTime defaultStartPoint + 0.1)

                        sectionRangeModel =
                            { model | section = Just (SectionRange { start = startPoint, end = Time (unwrapTime duration - 0.1) }) }

                        expected =
                            { sectionRangeModel | currentTime = startPoint }
                    in
                    doTest sectionRangeModel startPoint expected
            ]
        ]

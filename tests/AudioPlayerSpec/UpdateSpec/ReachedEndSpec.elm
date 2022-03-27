module AudioPlayerSpec.UpdateSpec.ReachedEndSpec exposing (..)

import AudioPlayer exposing (Model, Msg(..), Section(..), Source, defaultStartPoint, initModel, update)
import Expect
import Ports exposing (seek)
import Test exposing (..)


doTest : Model -> Float -> Model -> Expect.Expectation
doTest model startPoint expected =
    let
        cmd =
            seek startPoint
    in
    Expect.equal (update ReachedEnd model) ( expected, cmd )


expectRollbackToDefaultStartPoint : Model -> Model -> Expect.Expectation
expectRollbackToDefaultStartPoint model expected =
    doTest model defaultStartPoint expected

reachedEndSpec : Test
reachedEndSpec =
    let
        duration =
            100.0

        durationDetectedSource =
            Source "x" "y" (Just duration)

        m =
            initModel durationDetectedSource.name durationDetectedSource.url

        model =
            { m | source = durationDetectedSource }
    in
    describe "ReachedEnd msg"
        [ describe "rollbacks to default startpoint"
            [ test "when section setting is none" <|
                \_ ->
                    let
                        noSectionModel =
                            { model | section = Nothing }

                        expected =
                            { noSectionModel | currentTime = defaultStartPoint }
                    in
                    expectRollbackToDefaultStartPoint noSectionModel expected
            , test "when section setting is only endpoint" <|
                \_ ->
                    let
                        sectionEndOnlyModel =
                            { model | section = Just (SectionEndOnly (defaultStartPoint + 0.1)) }

                        expected =
                            { sectionEndOnlyModel | currentTime = defaultStartPoint }
                    in
                    expectRollbackToDefaultStartPoint sectionEndOnlyModel expected
            ]
        , describe "rollbacks to user defined startpoint"
            [ test "when section setting is only startpoint" <|
                \_ ->
                    let
                        startPoint =
                            defaultStartPoint + 10

                        sectionStartOnlyModel =
                            { model | section = Just (SectionStartOnly startPoint) }

                        expected =
                            { sectionStartOnlyModel | currentTime = startPoint }
                    in
                    doTest sectionStartOnlyModel startPoint expected
            , test "when section range is set" <|
                \_ ->
                    let
                        startPoint =
                            defaultStartPoint + 10
                        sectionRangeModel =
                            { model | section = Just (SectionRange { start = startPoint, end = duration - 0.1})}
                        expected =
                            { sectionRangeModel | currentTime = startPoint}
                    in
                    doTest sectionRangeModel startPoint expected
            ]
        ]

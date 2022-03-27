module AudioPlayerSpec.UpdateSpec.ReachedEndSpec exposing (..)

import AudioPlayer exposing (Model, Msg(..), Section(..), Source, defaultStartPoint, initModel, update)
import Expect
import Ports exposing (seek)
import Test exposing (..)


expectRollbackStartPoint : Model -> Model -> Expect.Expectation
expectRollbackStartPoint model expected =
    let
        cmd =
            seek defaultStartPoint
    in
    Expect.equal (update ReachedEnd model) ( expected, cmd )


reachedEndSpec : Test
reachedEndSpec =
    let
        durationDetectedSource =
            Source "x" "y" (Just 100.9)

        m =
            initModel durationDetectedSource.name durationDetectedSource.url

        model =
            { m | source = durationDetectedSource }
    in
    describe "ReachedEnd msg"
        [ describe "rollbacks to startpoint"
            [ test "when section setting is none" <|
                \_ ->
                    let
                        noSectionModel =
                            { model | section = Nothing }

                        expected =
                            { noSectionModel | currentTime = defaultStartPoint }
                    in
                    expectRollbackStartPoint noSectionModel expected
            , test "when section setting is only endpoint" <|
                \_ ->
                    let
                        sectionEndOnlyModel =
                            { model | section = Just (SectionEndOnly (defaultStartPoint + 0.1)) }

                        expected =
                            { sectionEndOnlyModel | currentTime = defaultStartPoint }
                    in
                    expectRollbackStartPoint sectionEndOnlyModel expected
            ]
        ]

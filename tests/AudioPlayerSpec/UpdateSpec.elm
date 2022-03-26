module AudioPlayerSpec.UpdateSpec exposing (..)

import AudioPlayer exposing (Msg(..), Source, defaultStartPoint, initModel, update)
import Expect
import Ports exposing (seek)
import Test exposing (..)


reachedEndSpec : Test
reachedEndSpec =
    let
        durationDetectedSource =
            Source "x" "y" (Just 100.9)

        m =
            initModel durationDetectedSource.name durationDetectedSource.url

        model =
            { m | source = durationDetectedSource }

        msg =
            ReachedEnd
    in
    describe "ReachedEnd msg"
        [ describe "rollbacks to startpoint"
            [ test "when section setting is none" <|
                \_ ->
                    let
                        noSectionModel =
                            { model | section = Nothing }

                        cmd =
                            seek defaultStartPoint

                        expected =
                            ( { noSectionModel | currentTime = defaultStartPoint }, cmd )

                        actual =
                            update msg model
                    in
                    Expect.equal expected actual
            ]
        ]

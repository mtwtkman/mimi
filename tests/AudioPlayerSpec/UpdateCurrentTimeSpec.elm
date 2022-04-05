module AudioPlayerSpec.UpdateCurrentTimeSpec exposing (..)

import AudioPlayer exposing (Model, Msg(..), Time(..), defaultStartPoint, unwrapTime, update)
import Expect
import Fuzz
import Test exposing (..)
import TestUtil.Fuzzer exposing (duration, initialModel)
import TestUtil.Transform exposing (fixDuration)


durationFixedModel : Fuzz.Fuzzer Model
durationFixedModel =
    Fuzz.map2
        fixDuration
        duration
        initialModel


suite : Test
suite =
    describe "UpdateCurrentTime msg"
        [ fuzz durationFixedModel "rollback startpoint by reached end" <|
            \model ->
                let
                    durationValue =
                        unwrapTime (Maybe.withDefault (Time -1.0) model.source.duration)

                    actual =
                        update (UpdatedCurrentTime durationValue) model

                    expected =
                        ( { model | currentTime = defaultStartPoint }, Cmd.none )
                in
                Expect.equal actual expected
        ]

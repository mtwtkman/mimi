module TestUtil.Fuzzer exposing
    ( duration
    , durationFixedModel
    , initialModel
    )

import AudioPlayer
    exposing
        ( Model
        , Name(..)
        , Time(..)
        , Url(..)
        , initModel
        , unwrapDuration
        )
import Fuzz
import Random
import Shrink
import TestUtil.Generator as Gen
import TestUtil.Shrink as S exposing (name, time, url)
import TestUtil.Transform exposing (fixDuration)


duration : Fuzz.Fuzzer Time
duration =
    Fuzz.custom Gen.duration time


initialModel : Fuzz.Fuzzer Model
initialModel =
    Fuzz.custom
        Gen.initialModel
        (\model ->
            Shrink.map initModel (name model.source.name)
                |> Shrink.andMap (url model.source.url)
        )


durationFixedModel : Fuzz.Fuzzer Model
durationFixedModel =
    Fuzz.custom Gen.durationFixModel S.model

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
        )
import Fuzz
import Shrink
import TestUtil.Generator as Gen
import TestUtil.Shrink as S exposing (name, time, url)


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

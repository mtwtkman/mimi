module TestUtil.Fuzzer exposing
    ( duration
    , initialModel
    )

import AudioPlayer exposing
    ( Model
    , Time(..)
    , Name(..)
    , Url(..)
    , initModel
    )
import TestUtil.Generator as Gen
import Fuzz
import TestUtil.Shrink exposing (name, url, time)
import Shrink

duration : Fuzz.Fuzzer Time
duration =
    Fuzz.custom Gen.duration time


initialModel : Fuzz.Fuzzer Model
initialModel =
    Fuzz.custom
        Gen.initialModel
        (\model ->
            Shrink.map initModel ( name model.source.name )
                |> Shrink.andMap (url model.source.url)
        )
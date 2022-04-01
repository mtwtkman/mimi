module TestUtil.Generator exposing (..)

import AudioPlayer exposing (Time(..), unwrapTime)
import Shrink exposing (Shrinker)
import Random


boolGenerator : Random.Generator Bool
boolGenerator =
    Random.uniform True [ False ]


maxDuration : Time
maxDuration =
    Time 100.0


minDuration : Time
minDuration =
    Time 1.0


durationGenerator : Random.Generator Float
durationGenerator =
    Random.float (unwrapTime minDuration) (unwrapTime maxDuration)


timeGenerator : Random.Generator Time
timeGenerator =
    Random.map Time durationGenerator


timeShrinker : Shrinker Time
timeShrinker (Time t) =
    Shrink.map Time (Shrink.float t)
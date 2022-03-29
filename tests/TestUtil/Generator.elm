module TestUtil.Generator exposing (..)

import AudioPlayer exposing (Time)
import Random


boolGenerator : Random.Generator Bool
boolGenerator =
    Random.uniform True [ False ]


maxDuration : Float
maxDuration =
    100.0


minDuration : Float
minDuration =
    1.0


durationGenerator : Random.Generator Float
durationGenerator =
    Random.float minDuration maxDuration


timeGenerator : Random.Generator Time
timeGenerator =
    Random.map Time durationGenerator

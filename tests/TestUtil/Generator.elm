module TestUtil.Generator exposing (..)

import AudioPlayer exposing (Name(..), Time(..), Url(..), unwrapTime)
import Random
import Random.Char exposing (ascii, unicode)
import Random.String exposing (rangeLengthString, string)
import Shrink exposing (Shrinker)


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


nameGenerator : Random.Generator Name
nameGenerator =
    Random.map Name (rangeLengthString 10 30 unicode)


urlGenerator : Random.Generator Url
urlGenerator =
    Random.map Url (string 100 ascii)

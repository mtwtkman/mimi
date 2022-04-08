module TestUtil.Shrink exposing (..)

import AudioPlayer
    exposing
        ( Model
        , Name(..)
        , Time(..)
        , Url(..)
        )
import Shrink exposing (Shrinker)


time : Shrinker Time
time (Time t) =
    Shrink.map Time (Shrink.float t)


name : Shrinker Name
name (Name n) =
    Shrink.map Name (Shrink.string n)


url : Shrinker Url
url (Url u) =
    Shrink.map Url (Shrink.string u)


model : Shrinker Model
model =
    Shrink.noShrink

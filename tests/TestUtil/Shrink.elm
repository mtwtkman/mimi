module TestUtil.Shrink exposing (..)

import Shrink exposing (Shrinker)
import AudioPlayer exposing
    ( Time(..)
    , Name(..)
    , Url(..)
    )


time : Shrinker Time
time (Time t) =
    Shrink.map Time (Shrink.float t)

name : Shrinker Name
name (Name n) =
    Shrink.map Name (Shrink.string n)

url : Shrinker Url
url (Url u) =
    Shrink.map Url (Shrink.string u)
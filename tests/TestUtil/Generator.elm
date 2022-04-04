module TestUtil.Generator exposing (..)

import AudioPlayer exposing
    ( Name(..)
    , Time(..)
    , Url(..)
    , Model
    , unwrapTime
    , initModel
    , Section(..)
    , PlaybackRate(..)
    , Volume(..)
    , Source
    , State(..)
    )
import Random
import Random.Char exposing (ascii, unicode)
import Random.Extra exposing (bool, maybe)
import Random.String exposing (rangeLengthString, string)


loop : Random.Generator Bool
loop = bool

playbackRate : Random.Generator PlaybackRate
playbackRate =
    Random.map PlaybackRate (Random.float 0.1 2.0)


volume : Random.Generator Volume
volume =
    Random.map Volume (Random.int 0 100)

duration : Random.Generator Time
duration =
    Random.map Time (Random.float 1.0 999.0)

source : Random.Generator Source
source =
    Random.map3
        Source
        name
        url
        (maybe bool duration)


state : Random.Generator State
state =
    Random.uniform Playing [Paused]

name : Random.Generator Name
name =
    Random.map Name (rangeLengthString 10 30 unicode)


url : Random.Generator Url
url =
    Random.map Url (string 100 ascii)

section : Time -> Random.Generator Section
section dur =
    let
        rawDuration = unwrapTime dur
        timeGen : Time -> Time -> Random.Generator Time
        timeGen s e =
            Random.map Time (Random.float (unwrapTime s) (unwrapTime e))

        startLimit = Time (rawDuration / 2.0)
        startGen = timeGen (Time 0.0) startLimit
        endGen = timeGen startLimit dur
    in
    Random.andThen identity <|
        Random.uniform
            (Random.map SectionStartOnly startGen)
            [ Random.map SectionEndOnly endGen
            , Random.map2 (\s -> \e -> SectionRange { start = s, end = e }) startGen endGen
            ]

initialModel : Random.Generator Model
initialModel =
    Random.map2 initModel name url
module TestUtil.Transform exposing
    ( changePlaybackRate
    , changeVolume
    , disableLoop
    , enableLoop
    , fixDuration
    , lostDuration
    , occurError
    , pause
    , play
    , resetSource
    , resolveError
    , seek
    , setEndOnly
    , setLoop
    , setSectionRange
    , setStartOnly
    , unsetSection
    )

import AudioPlayer
    exposing
        ( AudioPlayerError
        , Model
        , Msg(..)
        , Name
        , PlaybackRate
        , Section(..)
        , Source
        , State(..)
        , Time
        , Url
        , Volume
        )


setLoop : Bool -> Model -> Model
setLoop loop model =
    { model | loop = loop }


enableLoop : Model -> Model
enableLoop model =
    setLoop True model


disableLoop : Model -> Model
disableLoop model =
    setLoop False model


setStartOnly : Time -> Model -> Model
setStartOnly t model =
    { model | section = Just (SectionStartOnly t) }


setEndOnly : Time -> Model -> Model
setEndOnly t model =
    { model | section = Just (SectionEndOnly t) }


setSectionRange : Time -> Time -> Model -> Model
setSectionRange s e model =
    { model | section = Just (SectionRange { start = s, end = e }) }


unsetSection : Model -> Model
unsetSection model =
    { model | section = Nothing }


resetSource : Name -> Url -> Maybe Time -> Model -> Model
resetSource name url duration model =
    { model | source = Source name url duration }


fixDuration : Time -> Model -> Model
fixDuration duration model =
    resetSource model.source.name model.source.url (Just duration) model


lostDuration : Model -> Model
lostDuration model =
    resetSource model.source.name model.source.url Nothing model


play : Model -> Model
play model =
    { model | state = Playing }


pause : Model -> Model
pause model =
    { model | state = Paused }


changeVolume : Volume -> Model -> Model
changeVolume volume model =
    { model | volume = volume }


changePlaybackRate : PlaybackRate -> Model -> Model
changePlaybackRate playbackRate model =
    { model | playbackRate = playbackRate }


seek : Time -> Model -> Model
seek t model =
    { model | currentTime = t }


setError : Maybe AudioPlayerError -> Model -> Model
setError error model =
    { model | error = error }


resolveError : Model -> Model
resolveError model =
    setError Nothing model


occurError : AudioPlayerError -> Model -> Model
occurError e model =
    setError (Just e) model

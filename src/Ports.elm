port module Ports exposing (..)


-- PORTS

port play : () -> Cmd msg


port pause : () -> Cmd msg


port changeVolume : Int -> Cmd msg


port changePlaybackRate : Float -> Cmd msg


port updateCurrentTime : () -> Cmd msg


port touchVolumeSlider : () -> Cmd msg

port moveVolumeSlider : () -> Cmd msg

port untouchVolumeSlider : () -> Cmd msg

-- SUBSCRIPTIONS


port currentTimeReciever : (Float -> msg) -> Sub msg


port currentVolumeReciever : (Int -> msg ) -> Sub msg
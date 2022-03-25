port module Ports exposing (..)

-- PORTS


port play : () -> Cmd msg


port pause : () -> Cmd msg


port changeVolume : () -> Cmd msg


port changePlaybackRate : Float -> Cmd msg

port seek : Float -> Cmd msg


port updateCurrentTime : () -> Cmd msg

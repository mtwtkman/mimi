port module Ports exposing (..)


port play : () -> Cmd msg


port pause : () -> Cmd msg


port setVolume : Float -> Cmd msg


port setSpeed : Float -> Cmd msg


port updateCurrentTime : () -> Cmd msg


port currentTimeReciever : (Float -> msg) -> Sub msg

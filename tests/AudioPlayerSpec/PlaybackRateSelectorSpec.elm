module AudioPlayerSpec.PlaybackRateSelectorSpec exposing (..)

import AudioPlayer
    exposing
        ( Msg(..)
        , playbackRateChoices
        , playbackRateSelector
        , unwrapPlaybackRate
        , Model
        )
import Html.Attributes exposing (value)
import Html.Styled exposing (toUnstyled)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import TestUtil.Fuzzer exposing (initialModel)

testableElem : Model -> Query.Single Msg
testableElem model = playbackRateSelector model |> toUnstyled |> Query.fromHtml

suite : Test
suite =
    describe "playbackRateSelector"
        [ fuzz initialModel "selects default playback rate related from model" <|
            \model ->
                testableElem model
                    |> Query.find [ Selector.selected True ]
                    |> Query.has [ Selector.attribute (value (String.fromFloat (unwrapPlaybackRate model.playbackRate))) ]
        , fuzz initialModel "emits a msg with option value" <|
            \model ->
                let
                    choices =
                        List.filter (\v -> unwrapPlaybackRate model.playbackRate /= v) playbackRateChoices

                    selectedValue =
                        Maybe.withDefault 0.0 (List.head choices)
                in
                testableElem model
                    |> Query.find [ Selector.tag "select" ]
                    |> Event.simulate (String.fromFloat selectedValue |> Event.input)
                    |> Event.expect (ChangedPlaybackRate <| String.fromFloat selectedValue)
        ]

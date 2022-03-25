module AudioPlayer.PlaybackRateSelectorSpec exposing (..)

import AudioPlayer
    exposing
        ( Msg(..)
        , initModel
        , playbackRateChoices
        , playbackRateSelector
        )
import Html.Attributes exposing (value)
import Html.Styled exposing (toUnstyled)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector


suite : Test
suite =
    let
        model =
            initModel "x" "y"

        testableElem =
            playbackRateSelector model |> toUnstyled |> Query.fromHtml
    in
    describe "playbackRateSelector"
        [ test "selects default playback rate related from model" <|
            \_ ->
                testableElem
                    |> Query.find [ Selector.selected True ]
                    |> Query.has [ Selector.attribute (value (String.fromFloat model.playbackRate)) ]
        , test "emits a msg with option value" <|
            \_ ->
                let
                    choices =
                        List.filter (\v -> model.playbackRate /= v) playbackRateChoices

                    selectedValue =
                        Maybe.withDefault 0.0 (List.head choices)
                in
                testableElem
                    |> Event.simulate (String.fromFloat selectedValue |> Event.input)
                    |> Event.expect (ChangedPlaybackRate <| String.fromFloat selectedValue)
        ]

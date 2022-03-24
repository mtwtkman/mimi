module AudioPlayer.PlaybackRateSelectorSpec exposing (..)

import AudioPlayer
    exposing
        ( Msg(..)
        , initModel
        , playbackRateChoices
        , playbackRateSelector
        )
import Expect
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

        elem =
            playbackRateSelector model |> toUnstyled
    in
    describe "playbackRateSelector"
        [ test "selects default playback rate related from model" <|
            \_ ->
                elem
                    |> Query.fromHtml
                    |> Query.find [ Selector.selected True ]
                    |> Query.has [ Selector.attribute (value (String.fromFloat model.playbackRate)) ]
        , test "updates model by selected value" <|
            \_ ->
                let
                    choices =
                        List.filter (\v -> model.playbackRate /= v) playbackRateChoices

                    selectedValue =
                        Maybe.withDefault 0.0 (List.head choices)
                in
                elem
                    |> Query.fromHtml
                    |> Event.simulate (String.fromFloat selectedValue |> Event.input)
                    |> Event.expect (SelectedPlaybackRate <| String.fromFloat selectedValue)
        ]

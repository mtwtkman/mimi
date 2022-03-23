module AudioPlayer exposing
    ( Model
    , Msg(..)
    , Source
    , initModel
    , update
    , view
    )

import Html exposing (Attribute, Html, audio, br, div, i, input, text)
import Html.Attributes as Attr exposing (class, controls, src, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as D
import Ports
    exposing
        ( changeVolume
        , pause
        , play
        , updateCurrentTime
        )


type alias Source =
    { name : String
    , url : String
    , duration : Maybe Float
    }


initSource : String -> String -> Source
initSource name url =
    Source name url Nothing


type State
    = Playing
    | Paused


isPlaying : State -> Bool
isPlaying state =
    state == Playing


type alias Volume = Int


type alias PlaybackRate = Float


type alias Section =
    { start : Float
    , end : Float
    }


type alias CurrentTime = Float


validateRange : comparable -> comparable -> comparable -> Bool
validateRange minV maxV v =
    minV <= v && v <= maxV


validateSection : Float -> Section -> Bool
validateSection duration section =
    validateRange 0.0 duration section.start
        && validateRange 0.0 duration section.end
        && section.start
        < section.end


type alias Model =
    { state : State
    , section : Maybe Section
    , source : Source
    , playbackRate : PlaybackRate
    , loop : Bool
    , volume : Volume
    , currentTime : CurrentTime
    }


initModel : String -> String -> Model
initModel name url =
    Model
        Paused
        Nothing
        (initSource name url)
        1.0
        False
        30
        0.0


type SectionMsg
    = SetStartPoint Float
    | SetEndPoint Float


type Msg
    = Toggle
    | LoadedData Float
    | GotSectionMsg SectionMsg
    | ChangeVolume String
    | SetplaybackRate String
    | ClickedProgressBar
    | GotCurrentTime Float
    | GotCurrentVolume Int
    | TouchedVolumeSlider
    | MovedVolumeSlider
    | UntouchedVolumeSlider


updateSection : SectionMsg -> Float -> Section -> ( Section, Cmd msg )
updateSection msg duration section =
    case msg of
        SetStartPoint v ->
            let
                newSection =
                    { section | start = v }
            in
            if validateSection duration newSection then
                ( newSection, Cmd.none )

            else
                ( section, Cmd.none )

        SetEndPoint v ->
            let
                newSection =
                    { section | end = v }
            in
            if validateSection duration newSection then
                ( newSection, Cmd.none )

            else
                ( section, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle ->
            let
                ( newState, portCommand ) =
                    case model.state of
                        Playing ->
                            ( Paused, pause )

                        Paused ->
                            ( Playing, play )
            in
            ( { model | state = newState }, portCommand () )

        LoadedData duration ->
            let
                source =
                    model.source

                newSource =
                    { source | duration = Just duration }

                section =
                    Section 0.0 duration
            in
            ( { model | source = newSource, section = Just section }, Cmd.none )

        GotSectionMsg sectionMsg ->
            case ( model.source.duration, model.section ) of
                ( Just duration, Just section ) ->
                    let
                        ( newSection, subMsg ) =
                            updateSection sectionMsg duration section
                    in
                    ( { model | section = Just newSection }, Cmd.map GotSectionMsg subMsg )

                _ ->
                    ( model, Cmd.none )

        ChangeVolume v ->
            case String.toInt v of
                Just volume ->
                    ( { model | volume = volume }, changeVolume () )

                Nothing ->
                    ( model, Cmd.none )

        ClickedProgressBar ->
            ( model, updateCurrentTime () )

        GotCurrentTime t ->
            ( { model | currentTime = t }, Cmd.none )

        GotCurrentVolume v ->
            ( { model | volume = v }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ class "audio-player"
        ]
        [ audioSourceView model.source
        , playerWrapperView model
        , sourceInfoView model.source
        ]


audioSourceView : Source -> Html Msg
audioSourceView source =
    audio
        [ src source.url
        , controls False
        , onLoadedData
        , class "audio-source"
        ]
        []


playerWrapperView : Model -> Html Msg
playerWrapperView model =
    div
        [ class "player"
        ]
        [ playerControlView model
        ]


playerControlView : Model -> Html Msg
playerControlView model =
    div
        [ class "player-control"
        ]
        [ playIconView model.state
        , volumeSlider model
        ]


sourceInfoView : Source -> Html Msg
sourceInfoView source =
    div
        [ class "audio-info"
        ]
        [ text <| "filename: " ++ source.name
        , br [] []
        , text <| "duration: " ++ String.fromFloat (Maybe.withDefault 0.0 source.duration)
        ]


playIconView : State -> Html Msg
playIconView state =
    let
        buttonIcon =
            if isPlaying state then
                "fa-pause"

            else
                "fa-play"
    in
    i
        [ onClick Toggle
        , class <| "fa " ++ buttonIcon
        ]
        []


progressbar : Model -> Html Msg
progressbar model =
    div
        [ class "progressbar"
        , onClick ClickedProgressBar
        ]
        []


playbackRateSettingForm : Model -> Html Msg
playbackRateSettingForm model =
    input
        [ class "audio-playbackRate"
        , type_ "number"
        , Attr.min "0.0"
        , Attr.max "0.2"
        , value (String.fromFloat model.playbackRate)
        ]
        []


volumeSlider : Model -> Html Msg
volumeSlider model =
    div []
        [ input
            [ class "audio-volume-slider"
            , type_ "range"
            , value (String.fromInt model.volume)
            , Attr.max "100"
            , Attr.min "0"
            , onInput ChangeVolume
            ]
            []
        , text <| String.fromInt model.volume
        ]


onLoadedData : Attribute Msg
onLoadedData =
    on "loadeddata" (D.map LoadedData (D.at [ "target", "duration" ] D.float))

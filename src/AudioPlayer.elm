module AudioPlayer exposing
    ( Model
    , Msg(..)
    , Source
    , initModel
    , playbackRateChoices
    , playbackRateSelector
    , update
    , view
    )

import Html.Styled exposing (Attribute, Html, audio, br, div, i, input, option, select, text, span)
import Html.Styled.Attributes as Attr exposing (class, controls, disabled, selected, src, type_, value)
import Html.Styled.Events exposing (on, onClick, onInput)
import Json.Decode as D
import Ports
    exposing
        ( changePlaybackRate
        , changeVolume
        , pause
        , play
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


type alias Volume =
    Int


type alias PlaybackRate =
    Float


type alias Section =
    { start : Float
    , end : Float
    }


type alias CurrentTime =
    Float


validateSection : Float -> Section -> Bool
validateSection duration section =
    let
        validateRange : comparable -> comparable -> comparable -> Bool
        validateRange minV maxV v =
            minV <= v && v <= maxV
    in
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
    | GotCurrentTime Float
    | SelectedPlaybackRate String
    | UpdatedCurrentTime Float


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

        GotCurrentTime t ->
            ( { model | currentTime = t }, Cmd.none )

        SelectedPlaybackRate v ->
            case String.toFloat v of
                Just playbackRate ->
                    ( { model | playbackRate = playbackRate }, changePlaybackRate playbackRate )

                Nothing ->
                    ( model, Cmd.none )
        UpdatedCurrentTime t ->
            ( { model | currentTime = t}, Cmd.none)

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
        , onCurrentTimeUpdate
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
    <|
        List.foldr
            (\maybe acc ->
                case maybe of
                    Just e ->
                        e :: acc

                    Nothing ->
                        acc
            )
            []
            [ Just (playIconView model.state)
            , Just (volumeSlider model)
            , Just (playbackRateSelector model)
            , Maybe.andThen (\d -> Just (progressBar model.currentTime d)) model.source.duration
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


progressBar : Float -> Float -> Html Msg
progressBar currentTime duration =
    let
        currentTimeString = String.fromFloat currentTime
    in
    div
        []
        [ input
            [ class "progress"
            , disabled True
            , Attr.min "0.0"
            , Attr.max <| String.fromFloat duration
            , Attr.value currentTimeString
            , type_ "range"
            ]
            []
        , span
            []
            [ text currentTimeString
            ]
        ]


type PlaybackRateRange
    = MaxPlaybackRate
    | MinPlaybackRate


toFloat : PlaybackRateRange -> Float
toFloat x =
    case x of
        MaxPlaybackRate ->
            2.0

        MinPlaybackRate ->
            0.1


playbackRateChoices : List Float
playbackRateChoices =
    [ toFloat MinPlaybackRate
    , 0.25
    , 0.5
    , 0.75
    , 1.0
    , 1.25
    , 1.5
    , 1.75
    , toFloat MaxPlaybackRate
    ]


playbackRateSelector : Model -> Html Msg
playbackRateSelector model =
    let
        minVal =
            toFloat MinPlaybackRate

        maxVal =
            toFloat MaxPlaybackRate

        toString : Float -> String
        toString =
            String.fromFloat

        options : List (Html msg)
        options =
            List.map
                (\v ->
                    let
                        s =
                            toString v
                    in
                    option
                        [ value s
                        , selected (v == model.playbackRate)
                        ]
                        [ text s
                        ]
                )
                playbackRateChoices
    in
    select
        [ class "audio-playback-rate-selector"
        , (toString >> Attr.min) minVal
        , (toString >> Attr.max) maxVal
        , onInput SelectedPlaybackRate
        ]
        options


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

onCurrentTimeUpdate : Attribute Msg
onCurrentTimeUpdate =
    on "timeupdate" (D.map UpdatedCurrentTime (D.at ["target", "currentTime"] D.float))
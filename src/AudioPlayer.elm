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

import Html.Styled as StyledHtml exposing (Attribute, Html, audio, div, i, input, option, select, span, text)
import Html.Styled.Attributes as Attr exposing (class, controls, selected, src, step, type_, value)
import Html.Styled.Events exposing (on, onClick, onInput)
import Json.Decode as D
import Ports
    exposing
        ( changePlaybackRate
        , changeVolume
        , pause
        , play
        , seek
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


type Section
    = SectionStartOnly Float
    | SectionEndOnly Float
    | SectionRange
        { start : Float
        , end : Float
        }


toRecord : Section -> { start : Maybe Float, end : Maybe Float }
toRecord section =
    case section of
        SectionStartOnly s ->
            { start = Just s, end = Nothing }

        SectionEndOnly e ->
            { start = Nothing, end = Just e }

        SectionRange r ->
            { start = Just r.start, end = Just r.end }


type alias CurrentTime =
    Float


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
    | ResetStartPoint
    | ResetEndPoint


type Msg
    = Toggle
    | LoadedData Float
    | GotSectionMsg SectionMsg
    | ChangedVolume String
    | GotCurrentTime Float
    | ChangedPlaybackRate String
    | UpdatedCurrentTime Float
    | Seeked String


updateSection : SectionMsg -> Float -> Section -> ( Maybe Section, Cmd msg )
updateSection msg duration section =
    case msg of
        SetStartPoint s ->
            case section of
                SectionStartOnly _ ->
                    ( Just (SectionStartOnly s), Cmd.none )

                SectionEndOnly e ->
                    ( Just (SectionRange { start = s, end = e }), Cmd.none )

                SectionRange r ->
                    ( Just (SectionRange { r | start = s }), Cmd.none )

        SetEndPoint e ->
            if e > duration then
                ( Just section, Cmd.none )

            else
                case section of
                    SectionStartOnly s ->
                        ( Just (SectionRange { start = s, end = e }), Cmd.none )

                    SectionEndOnly _ ->
                        ( Just (SectionEndOnly e), Cmd.none )

                    SectionRange r ->
                        ( Just (SectionRange { r | end = e }), Cmd.none )

        ResetStartPoint ->
            case section of
                SectionRange r ->
                    ( Just (SectionEndOnly r.end), Cmd.none )

                _ ->
                    ( Nothing, Cmd.none )

        ResetEndPoint ->
            case section of
                SectionRange r ->
                    ( Just (SectionStartOnly r.start), Cmd.none )

                _ ->
                    ( Nothing, Cmd.none )


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
                    SectionRange { start = 0.0, end = duration }
            in
            ( { model | source = newSource, section = Just section }, Cmd.none )

        GotSectionMsg sectionMsg ->
            case ( model.source.duration, model.section ) of
                ( Just duration, Just section ) ->
                    let
                        ( newSection, subMsg ) =
                            updateSection sectionMsg duration section
                    in
                    ( { model | section = newSection }, Cmd.map GotSectionMsg subMsg )

                _ ->
                    ( model, Cmd.none )

        ChangedVolume v ->
            case String.toInt v of
                Just volume ->
                    ( { model | volume = volume }, changeVolume () )

                Nothing ->
                    ( model, Cmd.none )

        GotCurrentTime t ->
            ( { model | currentTime = t }, Cmd.none )

        ChangedPlaybackRate v ->
            case String.toFloat v of
                Just playbackRate ->
                    ( { model | playbackRate = playbackRate }, changePlaybackRate playbackRate )

                Nothing ->
                    ( model, Cmd.none )

        UpdatedCurrentTime t ->
            ( { model | currentTime = t }, Cmd.none )

        Seeked v ->
            case String.toFloat v of
                Just currentTime ->
                    ( { model | currentTime = currentTime }, seek currentTime )

                Nothing ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ class "audio-player"
        ]
        [ sourceInfoView model.source
        , audioSourceView model.source
        , playerWrapperView model
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
        currentTimeString =
            String.fromFloat currentTime
    in
    div
        []
        [ input
            [ class "progress"
            , Attr.min "0.0"
            , Attr.max <| String.fromFloat duration
            , Attr.value currentTimeString
            , type_ "range"
            , onInput Seeked
            , step "0.01"
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
    div
        []
        [ span [] [ text "playbackRate" ]
        , select
            [ class "audio-playback-rate-selector"
            , onInput ChangedPlaybackRate
            ]
            options
        ]


volumeSlider : Model -> Html Msg
volumeSlider model =
    div []
        [ span
            []
            [ text <| "volume"
            ]
        , input
            [ class "audio-volume-slider"
            , type_ "range"
            , value (String.fromInt model.volume)
            , Attr.max "100"
            , Attr.min "0"
            , onInput ChangedVolume
            ]
            []
        , text <| String.fromInt model.volume
        ]


sectionForm : Section -> Html Msg
sectionForm section =
    let
        { start, end } =
            toRecord section
    in
    div
        []
        [ sectionStartInput start
        , sectionEndInput end
        ]
        |> StyledHtml.map GotSectionMsg


optionalFloatInputNode : Maybe Float -> (String -> msg) -> Html msg
optionalFloatInputNode v msg =
    input
        [ onInput msg
        , type_ "number"
        , (Maybe.andThen (String.fromFloat >> Just) >> Maybe.withDefault "")  v |> value
        ]
        []


sectionInput : Maybe Float -> (Float -> SectionMsg) -> SectionMsg -> Html SectionMsg
sectionInput v setMsg resetMsg =
    optionalFloatInputNode v (\inputValue ->
        case String.toFloat inputValue of
            Just val ->
                setMsg val
            Nothing ->
                resetMsg
    )


sectionStartInput : Maybe Float -> Html SectionMsg
sectionStartInput v =
    sectionInput v SetStartPoint ResetStartPoint

sectionEndInput : Maybe Float -> Html SectionMsg
sectionEndInput v =
    sectionInput v SetEndPoint ResetEndPoint


onLoadedData : Attribute Msg
onLoadedData =
    on "loadeddata" (D.map LoadedData (D.at [ "target", "duration" ] D.float))


onCurrentTimeUpdate : Attribute Msg
onCurrentTimeUpdate =
    on "timeupdate" (D.map UpdatedCurrentTime (D.at [ "target", "currentTime" ] D.float))

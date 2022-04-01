module AudioPlayer exposing
    ( Model
    , Msg(..)
    , Section(..)
    , SectionMsg(..)
    , SectionValidationResult(..)
    , Source
    , Time
    , defaultStartPoint
    , initModel
    , playbackRateChoices
    , playbackRateSelector
    , update
    , updateSection
    , validateSection
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


defaultStartPoint : Float
defaultStartPoint =
    0.0


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


type alias Time =
    { value : Float }


type TimeConversionResult
    = EndPointOverDuration
    | NegativeTimeError
    | ValidTime Time


type Section
    = SectionStartOnly Time
    | SectionEndOnly Time
    | SectionRange
        { start : Time
        , end : Time
        }


type SectionValidationResult
    = InvertSectionRangeError
    | UpdateSectionOk Section
    | SectionDoesNotChange
    | CancelSectionSetting
    | InvalidTimeError TimeConversionResult


validateSection : Section -> SectionValidationResult
validateSection section =
    UpdateSectionOk section


toRecord : Section -> { start : Maybe Time, end : Maybe Time }
toRecord section =
    case section of
        SectionStartOnly s ->
            { start = Just s, end = Nothing }

        SectionEndOnly e ->
            { start = Nothing, end = Just e }

        SectionRange r ->
            { start = Just r.start, end = Just r.end }


type AudioPlayerError
    = SectionError SectionValidationResult


type alias Model =
    { state : State
    , section : Maybe Section
    , source : Source
    , playbackRate : PlaybackRate
    , loop : Bool
    , volume : Volume
    , currentTime : Time
    , error : Maybe AudioPlayerError
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
        (Time 0.0)
        Nothing


type SectionMsg
    = SetStartPoint Float
    | SetEndPoint Float
    | ResetStartPoint
    | ResetEndPoint


type Msg
    = Play
    | LoadedData Float
    | GotSectionMsg SectionMsg
    | ChangedVolume String
    | GotCurrentTime Float
    | ChangedPlaybackRate String
    | UpdatedCurrentTime Float
    | Seeked String
    | ToggleLoopSetting
    | ReachedEnd


toTime : Float -> Float -> TimeConversionResult
toTime v duration =
    if v < 0.0 then
        NegativeTimeError

    else if v > duration then
        EndPointOverDuration

    else
        ValidTime (Time v)


updateSection : SectionMsg -> Float -> Section -> ( SectionValidationResult, Cmd msg )
updateSection msg duration section =
    case msg of
        SetStartPoint v ->
            case toTime v duration of
                ValidTime t ->
                    ( UpdateSectionOk
                        (case section of
                            SectionStartOnly _ ->
                                SectionStartOnly t

                            SectionEndOnly e ->
                                SectionRange { start = t, end = e }

                            SectionRange r ->
                                SectionRange { r | start = t }
                        )
                    , Cmd.none
                    )

                err ->
                    ( InvalidTimeError err, Cmd.none )

        SetEndPoint v ->
            case toTime v duration of
                ValidTime t ->
                    if t.value > duration then
                        ( InvalidTimeError EndPointOverDuration, Cmd.none )

                    else
                        ( UpdateSectionOk
                            (case section of
                                SectionStartOnly s ->
                                    SectionRange { start = s, end = t }

                                SectionEndOnly _ ->
                                    SectionEndOnly t

                                SectionRange r ->
                                    SectionRange { r | end = t }
                            )
                        , Cmd.none
                        )

                err ->
                    ( InvalidTimeError err, Cmd.none )

        ResetStartPoint ->
            case section of
                SectionRange r ->
                    ( UpdateSectionOk (SectionEndOnly r.end), Cmd.none )

                SectionStartOnly _ ->
                    ( CancelSectionSetting, Cmd.none )

                SectionEndOnly _ ->
                    ( SectionDoesNotChange, Cmd.none )

        ResetEndPoint ->
            case section of
                SectionRange r ->
                    ( UpdateSectionOk (SectionStartOnly r.start), Cmd.none )

                SectionStartOnly _ ->
                    ( SectionDoesNotChange, Cmd.none )

                SectionEndOnly _ ->
                    ( CancelSectionSetting, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Play ->
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
                    SectionRange { start = Time defaultStartPoint, end = Time duration }
            in
            ( { model | source = newSource, section = Just section }, Cmd.none )

        GotSectionMsg sectionMsg ->
            case ( model.source.duration, model.section ) of
                ( Just duration, Just section ) ->
                    let
                        ( updateSectionResult, subMsg ) =
                            updateSection sectionMsg duration section

                        cmd =
                            Cmd.map GotSectionMsg subMsg
                    in
                    ( case updateSectionResult of
                        InvertSectionRangeError ->
                            { model | error = Just (SectionError InvertSectionRangeError) }

                        UpdateSectionOk s ->
                            { model | section = Just s }

                        SectionDoesNotChange ->
                            model

                        CancelSectionSetting ->
                            model

                        InvalidTimeError e ->
                            { model | error = Just (SectionError (InvalidTimeError e)) }
                    , cmd
                    )

                _ ->
                    ( model, Cmd.none )

        ChangedVolume v ->
            case String.toInt v of
                Just volume ->
                    ( { model | volume = volume }, changeVolume () )

                Nothing ->
                    ( model, Cmd.none )

        GotCurrentTime t ->
            ( { model | currentTime = Time t }, Cmd.none )

        ChangedPlaybackRate v ->
            case String.toFloat v of
                Just playbackRate ->
                    ( { model | playbackRate = playbackRate }, changePlaybackRate playbackRate )

                Nothing ->
                    ( model, Cmd.none )

        UpdatedCurrentTime t ->
            let
                cmd =
                    if t >= Maybe.withDefault defaultStartPoint model.source.duration then
                        Cmd.map (\_ -> ReachedEnd) Cmd.none

                    else
                        Cmd.none
            in
            ( { model | currentTime = Time t }, cmd )

        Seeked v ->
            case String.toFloat v of
                Just t ->
                    ( { model | currentTime = Time t }, seek t )

                Nothing ->
                    ( model, Cmd.none )

        ToggleLoopSetting ->
            ( { model | loop = not model.loop }, Cmd.none )

        ReachedEnd ->
            let
                startPoint =
                    case model.section of
                        Nothing ->
                            Time defaultStartPoint

                        Just (SectionEndOnly _) ->
                            Time defaultStartPoint

                        Just (SectionStartOnly s) ->
                            s

                        Just (SectionRange r) ->
                            r.start
            in
            ( { model | currentTime = startPoint }, seek startPoint.value )


view : Model -> Html Msg
view model =
    div
        [ class "audio-player"
        ]
        [ sourceInfoView model.source
        , audioSourceView model.source
        , playerWrapperView model
        , loopModelCheckBox model.loop
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


andThenRender : Maybe a -> (a -> Html Msg) -> Maybe (Html Msg)
andThenRender a v =
    Maybe.andThen (v >> Just) a


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
            , andThenRender model.source.duration (progressBar model.currentTime)
            , andThenRender model.section sectionForm
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
        [ onClick Play
        , class <| "fa " ++ buttonIcon
        ]
        []


progressBar : Time -> Float -> Html Msg
progressBar currentTime duration =
    let
        currentTimeString =
            String.fromFloat currentTime.value
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


optionalFloatInputNode : Maybe Time -> (String -> msg) -> Html msg
optionalFloatInputNode v msg =
    input
        [ onInput msg
        , type_ "number"
        , (Maybe.andThen (.value >> String.fromFloat >> Just) >> Maybe.withDefault "") v |> value
        ]
        []


sectionInput : Maybe Time -> (Float -> SectionMsg) -> SectionMsg -> Html SectionMsg
sectionInput v setMsg resetMsg =
    optionalFloatInputNode v
        (\inputValue ->
            case String.toFloat inputValue of
                Just val ->
                    setMsg val

                Nothing ->
                    resetMsg
        )


sectionStartInput : Maybe Time -> Html SectionMsg
sectionStartInput v =
    sectionInput v SetStartPoint ResetStartPoint


sectionEndInput : Maybe Time -> Html SectionMsg
sectionEndInput v =
    sectionInput v SetEndPoint ResetEndPoint


loopModelCheckBox : Bool -> Html Msg
loopModelCheckBox loopEnabled =
    div []
        [ text "loop enabled"
        , input
            [ type_ "checkbox"
            , Attr.checked loopEnabled
            , onInput (\_ -> ToggleLoopSetting)
            ]
            []
        ]


onLoadedData : Attribute Msg
onLoadedData =
    on "loadeddata" (D.map LoadedData (D.at [ "target", "duration" ] D.float))


onCurrentTimeUpdate : Attribute Msg
onCurrentTimeUpdate =
    on "timeupdate" (D.map UpdatedCurrentTime (D.at [ "target", "currentTime" ] D.float))

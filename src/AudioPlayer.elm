module AudioPlayer exposing
    ( AudioPlayerError(..)
    , Model
    , Msg(..)
    , Name(..)
    , PlaybackRate(..)
    , Section(..)
    , SectionMsg(..)
    , SectionValidationResult(..)
    , Source
    , State(..)
    , Time(..)
    , Url(..)
    , Volume(..)
    , defaultStartPoint
    , initModel
    , playbackRateChoices
    , playbackRateSelector
    , reachEnd
    , unwrapPlaybackRate
    , unwrapTime
    , update
    , updateSection
    , validateSection
    , view
    )

import Html.Styled as StyledHtml exposing (Attribute, Html, audio, div, i, input, option, select, span, text)
import Html.Styled.Attributes as Attr exposing (class, controls, selected, src, step, type_, value)
import Html.Styled.Events exposing (on, onClick, onInput)
import Json.Decode as D
import Ports as P
    exposing
        ( changePlaybackRate
        , changeVolume
        , seek
        )


defaultStartPoint : Time
defaultStartPoint =
    Time 0.0


type Name
    = Name String


type Url
    = Url String


type alias Source =
    { name : Name
    , url : Url
    , duration : Maybe Time
    }


unwrapName : Name -> String
unwrapName (Name a) =
    a


unwrapUrl : Url -> String
unwrapUrl (Url a) =
    a


unwrapTime : Time -> Float
unwrapTime (Time a) =
    a


unwrapVolume : Volume -> Int
unwrapVolume (Volume a) =
    a


unwrapPlaybackRate : PlaybackRate -> Float
unwrapPlaybackRate (PlaybackRate a) =
    a


initSource : Name -> Url -> Source
initSource name url =
    Source name url Nothing


type State
    = Playing
    | Paused


isPlaying : State -> Bool
isPlaying state =
    state == Playing


type Volume
    = Volume Int


type PlaybackRate
    = PlaybackRate Float


type Time
    = Time Float


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


initModel : Name -> Url -> Model
initModel name url =
    Model
        Paused
        Nothing
        (initSource name url)
        (PlaybackRate 1.0)
        False
        (Volume 30)
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


toTime : Float -> Time -> TimeConversionResult
toTime v duration =
    if v < 0.0 then
        NegativeTimeError

    else if v > unwrapTime duration then
        EndPointOverDuration

    else
        ValidTime (Time v)


updateSection : SectionMsg -> Time -> Section -> ( SectionValidationResult, Cmd msg )
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
                    if unwrapTime t > unwrapTime duration then
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


reachEnd : Model -> Model
reachEnd model =
    let
        startPoint =
            case model.section of
                Nothing ->
                    defaultStartPoint

                Just (SectionEndOnly _) ->
                    defaultStartPoint

                Just (SectionStartOnly s) ->
                    s

                Just (SectionRange r) ->
                    r.start
    in
    { model | currentTime = startPoint }


pause : Model -> Model
pause model =
    { model | state = Paused }


play : Model -> Model
play model =
    { model | state = Playing }


rollback : Model -> Model
rollback =
    reachEnd >> pause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Play ->
            let
                ( stateController, portCommand ) =
                    case model.state of
                        Playing ->
                            ( pause, P.pause )

                        Paused ->
                            ( play, P.play )
            in
            ( stateController model, portCommand () )

        LoadedData duration ->
            let
                d =
                    Time duration

                source =
                    model.source

                newSource =
                    { source | duration = Just d }

                section =
                    SectionRange { start = defaultStartPoint, end = d }
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
                    ( { model | volume = Volume volume }, changeVolume () )

                Nothing ->
                    ( model, Cmd.none )

        GotCurrentTime t ->
            ( { model | currentTime = Time t }, Cmd.none )

        ChangedPlaybackRate v ->
            case String.toFloat v of
                Just playbackRate ->
                    ( { model | playbackRate = PlaybackRate playbackRate }, changePlaybackRate playbackRate )

                Nothing ->
                    ( model, Cmd.none )

        UpdatedCurrentTime t ->
            let
                newModel =
                    if t >= (unwrapTime <| Maybe.withDefault defaultStartPoint model.source.duration) then
                        rollback model

                    else
                        { model | currentTime = Time t }
            in
            ( newModel, Cmd.none )

        Seeked v ->
            case String.toFloat v of
                Just t ->
                    ( { model | currentTime = Time t }, seek t )

                Nothing ->
                    ( model, Cmd.none )

        ToggleLoopSetting ->
            ( { model | loop = not model.loop }, Cmd.none )


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
        [ src <| unwrapUrl source.url
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
        [ text <| "filename: " ++ unwrapName source.name
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


progressBar : Time -> Time -> Html Msg
progressBar currentTime duration =
    let
        currentTimeString =
            String.fromFloat (unwrapTime currentTime)
    in
    div
        []
        [ input
            [ class "progress"
            , Attr.min "0.0"
            , Attr.max <| String.fromFloat (unwrapTime duration)
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
                        , selected (v == unwrapPlaybackRate model.playbackRate)
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
    let
        volume =
            unwrapVolume model.volume
    in
    div []
        [ span
            []
            [ text <| "volume"
            ]
        , input
            [ class "audio-volume-slider"
            , type_ "range"
            , value (String.fromInt volume)
            , Attr.max "100"
            , Attr.min "0"
            , onInput ChangedVolume
            ]
            []
        , text <| String.fromInt volume
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
        , (Maybe.andThen (unwrapTime >> String.fromFloat >> Just) >> Maybe.withDefault "") v |> value
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

module AudioPlayer exposing
    ( Error
    , ErrorCategory(..)
    , ErrorMemory
    , ErrorMemoryKey
    , ErrorMsg(..)
    , InvalidPlaybackRate(..)
    , InvalidVolume(..)
    , Model
    , Msg(..)
    , Source
    , errorMsgToErrorCategory
    , initModel
    , recordError
    , resolveError
    , toErrorMemoryKey
    , update
    , view
    )

import Dict
import Html exposing (Attribute, Html, audio, br, div, i, input, text)
import Html.Attributes as Attr exposing (class, controls, src, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as D
import List
import Ports exposing (pause, play, setVolume, updateCurrentTime)


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
    { value : Float }


type alias PlaybackRate =
    { value : Float }


type alias Section =
    { start : Float
    , end : Float
    }


type alias CurrentTime =
    { value : Float }


validateRange : comparable -> comparable -> comparable -> Bool
validateRange minV maxV v =
    minV <= v && v <= maxV


validateVolume : Volume -> VolumeValidationResult
validateVolume v =
    if v.value < 0.0 then
        InvalidVolume UnderflowVolume

    else if v.value > 100.0 then
        InvalidVolume OverflowVolume

    else
        ValidVolume


type InvalidVolume
    = OverflowVolume
    | UnderflowVolume


type VolumeValidationResult
    = ValidVolume
    | InvalidVolume InvalidVolume


validatePlaybackRate : PlaybackRate -> PlaybackRateValidationResult
validatePlaybackRate v =
    if v.value < 0.0 then
        InvalidPlaybackRate UnderflowPlaybackRate

    else if v.value > 2.0 then
        InvalidPlaybackRate OverflowPlaybackRate

    else
        ValidPlaybackRate


type InvalidPlaybackRate
    = OverflowPlaybackRate
    | UnderflowPlaybackRate


type PlaybackRateValidationResult
    = ValidPlaybackRate
    | InvalidPlaybackRate InvalidPlaybackRate


validateSection : Float -> Section -> Bool
validateSection duration section =
    validateRange 0.0 duration section.start
        && validateRange 0.0 duration section.end
        && section.start
        < section.end


type alias Error =
    { reason : ErrorMsg
    , message : String
    }


type alias ErrorMemoryKey =
    String


type alias ErrorMemory =
    Dict.Dict ErrorMemoryKey Error


type alias Model =
    { state : State
    , section : Maybe Section
    , source : Source
    , playbackRate : PlaybackRate
    , loop : Bool
    , volume : Volume
    , currentTime : CurrentTime
    , errors : ErrorMemory
    }


initModel : String -> String -> Model
initModel name url =
    Model
        Paused
        Nothing
        (initSource name url)
        (PlaybackRate 1.0)
        False
        (Volume 1.0)
        (CurrentTime 0.0)
        Dict.empty


type SectionMsg
    = SetStartPoint Float
    | SetEndPoint Float


type ErrorMsg
    = VolumeError InvalidVolume
    | PlaybackRateError InvalidPlaybackRate
    | InvalidVolumeInputValueError String
    | InvalidPlaybackRateInputValueError String


type ErrorCategory
    = DisallowedVolumeValue
    | DisallowedPlaybackRateValue
    | MalformedVolumeInputValue
    | MalformedInvalidPlaybackRateInputValue


errorMsgToErrorCategory : ErrorMsg -> ErrorCategory
errorMsgToErrorCategory msg =
    case msg of
        VolumeError _ ->
            DisallowedVolumeValue

        PlaybackRateError _ ->
            DisallowedPlaybackRateValue

        InvalidVolumeInputValueError _ ->
            MalformedVolumeInputValue

        InvalidPlaybackRateInputValueError _ ->
            MalformedInvalidPlaybackRateInputValue


type Msg
    = Toggle
    | LoadedData Float
    | GotSectionMsg SectionMsg
    | SetVolume String
    | SetplaybackRate String
    | ClickedProgressBar
    | GotCurrentTime Float
    | GotErrorMsg ErrorMsg


handleError : ErrorMsg -> ( Error, Cmd msg )
handleError msg =
    case msg of
        VolumeError UnderflowVolume ->
            ( Error msg "volume must be more than 0.0", Cmd.none )

        VolumeError OverflowVolume ->
            ( Error msg "volume must be less than 2.0", Cmd.none )

        PlaybackRateError UnderflowPlaybackRate ->
            ( Error msg "playback rate must be more than 0.0", Cmd.none )

        PlaybackRateError OverflowPlaybackRate ->
            ( Error msg "playback rate be less than 2.0", Cmd.none )

        InvalidVolumeInputValueError value ->
            ( Error msg ("Volume must be 0.0 <= x <= 100.0, but " ++ value), Cmd.none )

        InvalidPlaybackRateInputValueError value ->
            ( Error msg ("Playback Rate must be 0.0 <= x <= 2.0, but " ++ value), Cmd.none )


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


disallowedVolumeValueKey : ErrorMemoryKey
disallowedVolumeValueKey =
    "VolErr"


disallowedPlaybackRateValueKey : ErrorMemoryKey
disallowedPlaybackRateValueKey =
    "PbRErr"


malformedVolumeInputValueKey : ErrorMemoryKey
malformedVolumeInputValueKey =
    "VolInputErr"


malformedPlaybackRateInputValueKey : ErrorMemoryKey
malformedPlaybackRateInputValueKey =
    "PbRInputErr"


toErrorMemoryKey : ErrorCategory -> ErrorMemoryKey
toErrorMemoryKey c =
    case c of
        DisallowedVolumeValue ->
            disallowedVolumeValueKey

        DisallowedPlaybackRateValue ->
            disallowedPlaybackRateValueKey

        MalformedVolumeInputValue ->
            malformedVolumeInputValueKey

        MalformedInvalidPlaybackRateInputValue ->
            malformedPlaybackRateInputValueKey


recordError : ErrorMemory -> Error -> ErrorMemory
recordError errors error =
    Dict.insert ((errorMsgToErrorCategory >> toErrorMemoryKey) error.reason) error errors


resolveError : ErrorMemory -> List ErrorCategory -> ErrorMemory
resolveError errors categories =
    let
        keys =
            List.map toErrorMemoryKey categories
    in
    Dict.filter (\k _ -> not (List.member k keys)) errors


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

        SetVolume value ->
            case String.toFloat value of
                Just v ->
                    let
                        volume =
                            Volume v
                    in
                    case validateVolume volume of
                        ValidVolume ->
                            ( { model | volume = volume, errors = resolveError model.errors [ DisallowedVolumeValue, MalformedVolumeInputValue ] }
                            , setVolume volume.value
                            )

                        InvalidVolume a ->
                            let
                                ( error, subMsg ) =
                                    handleError (VolumeError a)
                            in
                            ( { model | errors = recordError model.errors error }, Cmd.map GotErrorMsg subMsg )

                Nothing ->
                    let
                        ( error, subMsg ) =
                            handleError (InvalidVolumeInputValueError value)
                    in
                    ( { model | errors = recordError model.errors error }, Cmd.map GotErrorMsg subMsg )

        ClickedProgressBar ->
            ( model, updateCurrentTime () )

        GotCurrentTime t ->
            ( { model | currentTime = CurrentTime t }, Cmd.none )

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


errorMessageView : ErrorMemory -> Html msg
errorMessageView errors =
    div
        [ class "error-messages"
        ]
        (List.map (\v -> text v.message) (Dict.values errors))


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


progressBarView : Model -> Html Msg
progressBarView model =
    div
        [ class "progressbar"
        , onClick ClickedProgressBar
        ]
        []


playbackRateSettingView : Model -> Html Msg
playbackRateSettingView model =
    input
        [ class "audio-playbackRate"
        , type_ "number"
        , Attr.min "0.0"
        , Attr.max "0.2"
        , onInput SetVolume
        ]
        []


onLoadedData : Attribute Msg
onLoadedData =
    on "loadeddata" (D.map LoadedData (D.at [ "target", "duration" ] D.float))

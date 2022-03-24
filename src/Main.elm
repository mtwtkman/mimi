module Main exposing (main)

import Css exposing (backgroundColor, hex, width, height, px)
import AudioPlayer as AP
import Browser
import File exposing (File)
import File.Select as Select
import Html.Styled as StyledHtml exposing (Attribute, button, div, span, text, Html, toUnstyled)
import Html.Styled.Attributes exposing (class, css)
import Html.Styled.Events exposing (onClick, preventDefaultOn)
import Json.Decode as D
import Ports exposing (currentTimeReciever, currentVolumeReciever)
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view >> toUnstyled
        , subscriptions = subscriptions
        }


type alias Model =
    { hover : Bool
    , audioPlayer : Maybe AP.Model
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model False Nothing, Cmd.none )


type Msg
    = GotFiles File (List File)
    | OpenFileSelector
    | DragEnter
    | DragLeave
    | BuildFileUrl ( String, File )
    | GotAudioPlayerMsg AP.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFiles file _ ->
            ( model, Task.perform BuildFileUrl (Task.map (\s -> ( s, file )) (File.toUrl file)) )

        OpenFileSelector ->
            ( model
            , Select.file
                [ "audio/basic"
                , "audio/L24"
                , "audio/mid"
                , "audio/mpeg"
                , "audio/mp4"
                , "audio/x-aiff"
                , "audio/x-mpegurl"
                , "audio/vnd.rn-realaudio"
                , "audio/ogg"
                , "audio/vorbis"
                , "audio/vnd.wav"
                ]
                (\f -> GotFiles f [])
            )

        DragEnter ->
            ( { model | hover = True }, Cmd.none )

        DragLeave ->
            ( { model | hover = False }, Cmd.none )

        BuildFileUrl ( url, fileObj ) ->
            let
                audioPlayerModel =
                    AP.initModel (File.name fileObj) url
            in
            ( { model | audioPlayer = Just audioPlayerModel }, Cmd.none )

        GotAudioPlayerMsg subMsg ->
            case model.audioPlayer of
                Nothing ->
                    ( model, Cmd.none )

                Just apModel ->
                    let
                        ( newApModel, apMsg ) =
                            AP.update subMsg apModel
                    in
                    ( { model | audioPlayer = Just newApModel }, Cmd.map GotAudioPlayerMsg apMsg )


view : Model -> Html Msg
view model =
    div []
        [ div
            [ class "audio-player-region"
            ]
            [ case model.audioPlayer of
                Just apModel ->
                    StyledHtml.map GotAudioPlayerMsg (AP.view apModel)

                Nothing ->
                    span [] []
            ]
        , div
            [ class "file-selector"
            ]
            [ button [ onClick OpenFileSelector ] [ text "select file" ]
            ]
        , div
            [ class "file-drop-area"
            , hijackOn "dragenter" (D.succeed DragEnter)
            , hijackOn "dragover" (D.succeed DragEnter)
            , hijackOn "dragleave" (D.succeed DragLeave)
            , hijackOn "drop" dropDecoder
            , css
                [ backgroundColor (hex (if model.hover then "336ff0" else "cacaca"))
                , width (px 400)
                , height (px 400)
                ]
            ]
            [ text "drop here your audio file" ]
        ]


dropDecoder : D.Decoder Msg
dropDecoder =
    D.at [ "dataTransfer", "files" ] (D.oneOrMore GotFiles File.decoder)


hijackOn : String -> D.Decoder msg -> Attribute msg
hijackOn event decoder =
    preventDefaultOn event (D.map hijack decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Sub.map GotAudioPlayerMsg (currentTimeReciever AP.GotCurrentTime)
        , Sub.map GotAudioPlayerMsg (currentVolumeReciever AP.GotCurrentVolume)
        ]

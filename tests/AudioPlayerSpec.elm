module AudioPlayerSpec exposing (suite)

import AudioPlayer as AP
import Dict
import Expect
import Test exposing (..)


suite : Test
suite =
    concat
        [ recordErrorSpec
        , resolveErrorSpec
        ]


emptyErrorMemory : AP.ErrorMemory
emptyErrorMemory =
    Dict.empty


errorMsgToKey : AP.ErrorMsg -> AP.ErrorMemoryKey
errorMsgToKey =
    AP.errorMsgToErrorCategory >> AP.toErrorMemoryKey


buildSingleError : AP.Error -> AP.ErrorMemory
buildSingleError e =
    Dict.singleton (errorMsgToKey e.reason) e


buildMultipleErrors : List AP.Error -> AP.ErrorMemory
buildMultipleErrors es =
    Dict.fromList <| List.map (\e -> ( errorMsgToKey e.reason, e )) es


recordErrorSpec : Test
recordErrorSpec =
    describe "recordError function"
        [ test "inserts new one" <|
            \_ ->
                let
                    errorMsg =
                        AP.InvalidVolumeInputValueError "x"

                    error =
                        AP.Error errorMsg "this is error"

                    actual =
                        AP.recordError emptyErrorMemory error

                    expected =
                        buildSingleError error
                in
                Expect.equalDicts actual expected
        , test "overwrite existed one" <|
            \_ ->
                let
                    errorMsg =
                        AP.InvalidVolumeInputValueError "a"

                    error =
                        AP.Error errorMsg "first"

                    errors =
                        buildSingleError error

                    newErrorMsg =
                        AP.InvalidVolumeInputValueError "b"

                    newError =
                        AP.Error newErrorMsg "second"

                    actual =
                        AP.recordError errors newError

                    expected =
                        buildSingleError newError
                in
                Expect.equalDicts actual expected
        ]


resolveErrorSpec : Test
resolveErrorSpec =
    describe "resolveError function"
        [ test "ignores unknown error " <|
            \_ ->
                Expect.true "Expected error memory keeps empty" <| Dict.isEmpty (AP.resolveError emptyErrorMemory [ AP.MalformedInvalidPlaybackRateInputValue ])
        , test "does nothing by passed empty error keys" <|
            \_ ->
                let
                    errorMsg =
                        AP.InvalidPlaybackRateInputValueError "a"

                    error =
                        AP.Error errorMsg "x"

                    errors =
                        buildSingleError error

                    actual =
                        AP.resolveError errors []
                in
                Expect.equalDicts actual errors
        , test "removes items by passed error list" <|
            \_ ->
                let
                    errorMsg1 =
                        AP.InvalidVolumeInputValueError "v"

                    error1 =
                        AP.Error errorMsg1 "remove-1"

                    errorMsg2 =
                        AP.InvalidPlaybackRateInputValueError "p"

                    error2 =
                        AP.Error errorMsg2 "remove-2"

                    removedErrors =
                        [ error1, error2 ]

                    errorMsg3 =
                        AP.VolumeError AP.OverflowVolume

                    error3 =
                        AP.Error errorMsg3 "keep"

                    keptErrors =
                        [ error3 ]

                    errors =
                        Dict.fromList (List.map (\e -> ( errorMsgToKey e.reason, e )) (removedErrors ++ keptErrors))

                    actual =
                        AP.resolveError errors (List.map AP.errorMsgToErrorCategory (List.map .reason removedErrors))

                    expected =
                        buildMultipleErrors keptErrors
                in
                Expect.equalDicts actual expected
        ]

module AudioPlayerSpec exposing (suite)

import AudioPlayer as AP
import Dict
import Expect
import Test exposing (..)


suite : Test
suite =
    concat
        [ recordErrorSuite
        ]

emptyErrorMemory : AP.ErrorMemory
emptyErrorMemory = Dict.empty

recordErrorSuite : Test
recordErrorSuite =
    let
        errorMsgToKey : AP.ErrorMsg -> AP.ErrorMemoryKey
        errorMsgToKey = AP.errorMsgToErrorCategory >> AP.toErrorMemoryKey

        singleError : AP.Error -> AP.ErrorMemory
        singleError e = Dict.singleton (errorMsgToKey e.reason) e
    in
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

                    expected = singleError error
                in
                Expect.equalDicts actual expected
        , test "overwrite existed one" <|
            \_ ->
                let
                    errorMsg =
                        AP.InvalidVolumeInputValueError "a"

                    error = AP.Error errorMsg "first"
                    errors = singleError error

                    newErrorMsg = AP.InvalidVolumeInputValueError "b"
                    newError = AP.Error newErrorMsg "second"
                    actual = AP.recordError errors newError
                    expected = singleError newError

                in
                Expect.equalDicts actual expected
        ]

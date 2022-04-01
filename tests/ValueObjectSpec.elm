module ValueObjectSpec exposing (..)

import Test exposing (..)
import Expect
import ValueObject exposing (ValueObject(..), validate, ValidateResult(..))

type PositiveIntItem = PositiveIntItem (ValueObject Int)

positiveIntItem : Int -> PositiveIntItem
positiveIntItem = ValueObject >> PositiveIntItem

unwrapPositiveIntItem : PositiveIntItem -> Int
unwrapPositiveIntItem (PositiveIntItem (ValueObject a)) = a

invalidPositiveIntItemErrorMessage : Int -> String
invalidPositiveIntItemErrorMessage = String.fromInt >> (++) "Value must be positive but: "

validatePositiveIntItem : PositiveIntItem -> ValidateResult Int String
validatePositiveIntItem (PositiveIntItem a) =
    let
        func : Int -> ValidateResult Int String
        func x =
            if x > 0 then ValueObject.Ok a
            else ValueObject.Err (invalidPositiveIntItemErrorMessage x)
    in
    validate func a

type RecordItem = RecordItem (ValueObject { x: Int, y: String })

recordItem : Int -> String -> RecordItem
recordItem x y = RecordItem (ValueObject { x = x, y = y })

unwrapRecordItem : RecordItem -> { x: Int, y: String }
unwrapRecordItem (RecordItem (ValueObject a)) = a

invalidRecordItemXErrorMessage : Int -> String
invalidRecordItemXErrorMessage = String.fromInt >> (++) "property `x` must be positive but: "

invalidRecordItemYErrorMessage : String
invalidRecordItemYErrorMessage = "property `y` must include some char but empty"

invalidRecordItemTotallyErrorMessage : Int -> String
invalidRecordItemTotallyErrorMessage = invalidRecordItemXErrorMessage >> (++) invalidRecordItemYErrorMessage

validateRecordItem : RecordItem -> ValidateResult { x: Int, y: String } String
validateRecordItem (RecordItem a) =
    let
        func : { x: Int, y: String } -> ValidateResult { x: Int, y: String } String
        func r =
            case (r.x < 0, String.length r.y == 0 ) of
                (True, True) -> ValueObject.Err (invalidRecordItemTotallyErrorMessage r.x)
                (False, True) -> ValueObject.Err invalidRecordItemYErrorMessage
                (True, False) -> ValueObject.Err (invalidRecordItemXErrorMessage r.x)
                (False, False) -> ValueObject.Ok a
    in
    validate func a

suite : Test
suite = describe "validate" <|
    [ test "works against a valid primitive value" <|
        \_ ->
            let
                correctItem = positiveIntItem 7

            in
            Expect.equal (validatePositiveIntItem correctItem) (ValueObject.Ok (ValueObject <| unwrapPositiveIntItem correctItem))
    , test "works against an invalid primitive value" <|
        \_ ->
            let
                incorrectItem = positiveIntItem -1
            in
            Expect.equal (validatePositiveIntItem incorrectItem) (ValueObject.Err (invalidPositiveIntItemErrorMessage <| unwrapPositiveIntItem incorrectItem))
    , test "works against a valid record " <|
        \_ ->
            let
                correctItem = recordItem 1 "a"
            in
            Expect.equal (validateRecordItem correctItem) (ValueObject.Ok (ValueObject <| unwrapRecordItem correctItem))
    ] ++
    ( List.map
        (\(item, errMsg) -> test ("works against an invalid record expected message:" ++ errMsg) <|
            \_ ->
                Expect.equal (validateRecordItem item) (ValueObject.Err errMsg)
        )
        [ (recordItem -1 "a", invalidRecordItemXErrorMessage -1)
        , (recordItem 1 "", invalidRecordItemYErrorMessage)
        , (recordItem -1 "", invalidRecordItemTotallyErrorMessage -1)
        ]
    )
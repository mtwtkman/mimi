module ValueObject exposing (..)

type ValueObject a = ValueObject a

type ValidateResult a e
    = Ok (ValueObject a)
    | Err e

validate : (a -> ValidateResult a e) -> ValueObject a -> ValidateResult a e
validate func (ValueObject a) = func a
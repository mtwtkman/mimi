module TestUtil exposing (..)

import Random

boolGenerator : Random.Generator Bool
boolGenerator =
    Random.uniform True [False]
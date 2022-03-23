#!/bin/sh

build() {
  elm make $1 src/Main.elm --output dist/app.js
  runghc Bundle
}

cmd=$1
shift
case $cmd in
  publish|p) build "--optimize";;
  dev|d) build "--debug";;
  serve|s)
    ./x dev
    python3 -m http.server -d mediaplayer 55301
    ;;
  reactor|w) elm reactor;;
  format|f) elm-format --yes src/* tests/*;;
  test|t) elm-test;;
  *) echo -n 'unknown command';
esac
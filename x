#!/bin/sh

builtjsname="dist/app.js"

cmd=$1
shift
case $cmd in
  publish|p)
    elm make --optimize src/Main.elm --output $builtjsname
    echo "uglify $builtjsname"
    npx uglify-js $builtjsname \
      --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' |\
       npx uglify-js --mangle --output $builtjsname
    runghc Bundle
    ;;
  dev|d)
    elm make --debug src/Main.elm --output $builtjsname
    runghc Bundle
    ;;
  serve|s)
    ./x dev
    python3 -m http.server -d mediaplayer 55301
    ;;
  reactor|w) elm reactor;;
  format|f) elm-format --yes src/*.elm tests/*;;
  test|t) elm-test;;
  *) echo -n 'unknown command';
esac
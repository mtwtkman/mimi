#!/bin/sh

distdir="dist"
appjs="app.js"
initjs="init.js"
builtappjs="$distdir/$appjs"
builtinitjs="$distdir/$initjs"

process_js() {
  mkdir -p $distdir
  elm make $1 src/Main.elm --output $builtappjs
  cp "src/$initjs" $builtinitjs
}

cmd=$1
shift
case $cmd in
  publish|p)
    process_js "--optimize"
    echo "uglify $builtappjs"
    npx uglifyjs $builtappjs \
      --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' |\
       npx uglifyjs --mangle --output $builtappjs
    echo "uglify $builtinitjs"
    npx uglifyjs --compress --mangle --output $builtinitjs -- $builtinitjs
    runghc Bundle
    ;;
  dev|d)
    process_js "--debug"
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
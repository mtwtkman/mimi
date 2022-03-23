module Main where

import Data.Char
import System.Directory
import Control.Monad

styleTag :: String -> String
styleTag src = "<link rel=\"stylesheet\" href=\"" ++ src ++ "\">"

stylesheet :: String
stylesheet = "style.css"

minifiedStyleSheet :: String
minifiedStyleSheet = "style.min.css"

buildHtml :: String -> String -> String
buildHtml appjsdef initjsdef = concat [
    "<html>"
    , "<head>"
    , "<meta charset=\"utf8\">"
    , styleTag "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"
    , styleTag ("./" ++ minifiedStyleSheet)
    , "<script>"
    , appjsdef
    , "</script></head>"
    , "<body><main></main><script>"
    , initjsdef
    , "</script></body>"
    ,"</html>"
    ]

readAppJavascript :: IO String
readAppJavascript =
    readFile "dist/app.js"

readInitJavascript :: IO String
readInitJavascript =
    readFile "src/init.js"

readStylesheet :: IO String
readStylesheet =
    readFile stylesheet

minifyStyleSheet :: String -> String
minifyStyleSheet = filter $ not . isSpace

buildStylesheet :: IO ()
buildStylesheet = do
    src <- readStylesheet
    writeFile (appDirectory ++ "/" ++ minifiedStyleSheet) (minifyStyleSheet src)

appDirectory :: FilePath
appDirectory = "./mediaplayer"

cleanup :: IO ()
cleanup =
    removeDirectoryRecursive appDirectory

setup :: IO ()
setup = do
    needToCleanup <-  doesDirectoryExist appDirectory
    when needToCleanup cleanup
    createDirectory appDirectory

indexHtml :: FilePath
indexHtml = appDirectory ++ "/index.html"

build :: String -> String -> IO ()
build appjsdef initjsdef = do
    writeFile indexHtml (buildHtml appjsdef initjsdef)

main :: IO ()
main = do
    setup
    buildStylesheet
    appjsdef <- readAppJavascript
    initjsdef <- readInitJavascript
    build appjsdef initjsdef
    putStr $ "Generated " ++ indexHtml
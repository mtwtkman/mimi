module Main where

import System.Directory
import Control.Monad

styleTag :: String -> String
styleTag src = "<link rel=\"stylesheet\" href=\"" ++ src ++ "\">"

myStyleSheet :: String
myStyleSheet = "style.css"

buildHtml :: String -> String -> String
buildHtml appjsdef initjsdef = concat [
    "<html>"
    , "<head>"
    , "<meta charset=\"utf8\">"
    , styleTag "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"
    , styleTag ("./" ++ myStyleSheet)
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

copyStyleSheet :: IO ()
copyStyleSheet =
    copyFile myStyleSheet (appDirectory ++ "/" ++ myStyleSheet)

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

filename :: FilePath
filename = appDirectory ++ "/index.html"

build :: String -> String -> IO ()
build appjsdef initjsdef = do
    writeFile filename (buildHtml appjsdef initjsdef)

main :: IO ()
main = do
    setup
    copyStyleSheet
    appjsdef <- readAppJavascript
    initjsdef <- readInitJavascript
    build appjsdef initjsdef
    putStr $ "Generated " ++ filename
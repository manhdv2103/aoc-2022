{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module AOC.IO (
  getIp,
  copyOp,
  submit,
  process
) where

import AOC.Utils
import Data.Char
import GHC.IO.Exception
import System.Directory
import System.IO
import System.Process
import Control.Monad

getCookie :: IO (String)
getCookie = fmap (reverse . dropWhile isSpace . reverse . dropWhile isSpace) $ readFile "../cookie" -- put your AOC session cookie in this file

getIp :: Int -> IO (String)
getIp day = do
  cached <- doesFileExist "input"
  if not cached then do
    cookie <- getCookie

    if null cookie then
      error "Missing cookie"
    else do
      prettyPrintLn "Downloading input..."
      (errCode, stdout', stderr') <- readProcessWithExitCode "curl" ["-b", cookie, "-A", "'curl by manhdv2103@gmail.com'", "https://adventofcode.com/2022/day/" ++ (prettyShow day) ++ "/input"] ""

      if errCode == ExitSuccess then do
        writeFile "input" stdout'
        return stdout'
      else
        error "Download input failed"
  else do
    contents <- readFile "input"
    return contents

copyOp :: Show a => a -> IO ()
copyOp content = do
  runCommand ("echo '" ++ (prettyShow content) ++ "' | xclip")
  return ()

submit :: Show a => Int -> Int -> a -> IO (String)
submit day part answer = do
  cookie <- getCookie

  if null cookie then
    error "Missing cookie"
  else do
    prettyPrintLn "Submitting answer..."
    (errCode, stdout', stderr') <- readProcessWithExitCode "bash" ["-c", "curl -b " ++ cookie ++ " -A 'curl by manhdv2103@gmail.com' -X POST --data 'level=" ++ (prettyShow part) ++ "&answer=" ++ (prettyShow answer) ++ "' 'https://adventofcode.com/2022/day/" ++ (prettyShow day) ++ "/answer' | xmllint --html --xpath 'normalize-space(//article/p)' - | sed 's/ \\[.*\\]//'"] ""

    if errCode == ExitSuccess then do
      let response = replace " If you're stuck, make sure you're using the full input data; there are also some general tips on the about page, or you can ask for hints on the subreddit. Please wait one minute before trying again." "" $ replace "; you have to wait after submitting an answer before trying again" "" stdout'
      return response
    else
      error "Download input failed"

result :: Show a => Int -> Int -> Int -> a -> IO ()
result day part willSubmit answer = do
  prettyPrintLn answer
  copyOp answer
  when (willSubmit == 1) $ do
    respond <- submit day part answer
    prettyPrintLn respond

process :: (Show a, Show b) => Int -> (String -> a) -> (String -> b) -> Int -> IO ()
process part solveP1 solveP2 willSubmit = do
  day <- fmap (read . pure . last) $ getCurrentDirectory
  input <- getIp day

  prettyPrintLn ("Day " ++ (prettyShow day))
  prettyPrintLn ("Part " ++ (prettyShow part) ++ ":")
  
  if part == 1 then
    result day part willSubmit $ solveP1 input
  else
    result day part willSubmit $ solveP2 input


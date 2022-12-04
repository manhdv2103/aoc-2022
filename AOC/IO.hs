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
      printr "Downloading input..."
      (errCode, stdout', stderr') <- readProcessWithExitCode "curl" ["-b", cookie, "-A", "'curl by manhdv2103@gmail.com'", "https://adventofcode.com/2022/day/" ++ (show day) ++ "/input"] ""

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
  runCommand ("echo '" ++ (show content) ++ "' | xclip")
  return ()

submit :: Show a => Int -> Int -> a -> IO (String)
submit day part answer = do
  cookie <- getCookie

  if null cookie then
    error "Missing cookie"
  else do
    printr "Submitting answer..."
    (errCode, stdout', stderr') <- readProcessWithExitCode "bash" ["-c", "curl -b " ++ cookie ++ " -A 'curl by manhdv2103@gmail.com' -X POST --data 'level=" ++ (show part) ++ "&answer=" ++ (show answer) ++ "' 'https://adventofcode.com/2022/day/" ++ (show day) ++ "/answer' | xmllint --html --xpath 'normalize-space(//article/p)' - | sed 's/ \\[.*\\]//'"] ""

    if errCode == ExitSuccess then do
      let response = replace " If you're stuck, make sure you're using the full input data; there are also some general tips on the about page, or you can ask for hints on the subreddit. Please wait one minute before trying again." "" $ replace "; you have to wait after submitting an answer before trying again" "" stdout'
      return response
    else
      error "Download input failed"

process :: Show a => Int -> (String -> a) -> (String -> a) -> Bool -> IO ()
process part solveP1 solveP2 willSubmit = do
  hSetBuffering stdout NoBuffering
  day <- fmap (read . pure . last) $ getCurrentDirectory
  input <- getIp day

  printr ("Day " ++ (show day))
  printr ("Part " ++ (show part) ++ ":")
  let answer = if part == 1 then solveP1 input else solveP2 input

  print answer
  copyOp answer
  when willSubmit $ do
    respond <- submit day part answer
    printr respond
    

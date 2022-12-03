module AOC.IO (
  getIp,
  copyOp,
  process
) where

import AOC.Utils
import Data.Char
import GHC.IO.Exception
import System.Directory
import System.Process

getIp :: Int -> IO (String)
getIp day = do
  cached <- doesFileExist "input"
  if not cached then do
    cookie <- fmap (reverse . dropWhile isSpace . reverse . dropWhile isSpace) $ readFile "../cookie" -- put your AOC session cookie in this file

    if null cookie then
      error "Missing cookie"
    else do
      printr "Downloading input..."
      (errCode, stdout', stderr') <- readProcessWithExitCode "curl" ["-b", cookie, "-A", "'curl by manhvd2103@gmail.com'", "https://adventofcode.com/2022/day/" ++ (show day) ++ "/input"] ""
      writeFile "input" stdout'

      if errCode == ExitSuccess then
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

process :: (Show a, Show b) => Int -> (String -> a) -> (String -> b) -> IO ()
process part solveP1 solveP2 = do
  day <- fmap (read . pure . last) $ getCurrentDirectory
  input <- getIp day

  printr ("Day " ++ (show day))
  printr ("Part " ++ (show part) ++ ":")
  if part == 1
  then do
    let res = solveP1 input
    print $ res
    copyOp res
  else do
    let res = solveP2 input
    print $ res
    copyOp res


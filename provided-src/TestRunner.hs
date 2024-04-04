module Main where

import Control.Exception
import Control.Monad
import Data.Char (isDigit)
import Data.List (intercalate, partition)
import Data.Word
import System.Environment
import System.Exit
import System.IO
import System.Process
import Text.Read (readMaybe)

type Test = (String, [Word32], [Word32])

runTest :: Test -> IO Bool
runTest (testSource, args, expected) = 
  do (_, Just out, _, theProcess) <- createProcess (proc "cabal" ("run" : "--verbose=0" : "Wasm" : testSource : map show args)){std_out = CreatePipe}
     catch (do output <- hGetLine out     
               let actual = map read (words output)
                   succeeded = actual == expected
               unless succeeded $ 
                 hPutStrLn stderr $ unwords $ ["When running test", testSource] ++
                                              (if null args then ["with no arguments"] else "with arguments" : map show args) ++
                                              ["expected", intercalate " " (map show expected), "but got", intercalate " " (map show actual)]
               return succeeded)
           (\e -> do hPutStrLn stderr $ unwords $ ["Unexpected failure running test", testSource] ++
                                                  if null args then ["with no arguments"] else "with arguments" : map show args
                     hPutStrLn stderr $ show (e :: IOException)
                     return False)

main :: IO ()
main = do
  allArgs <- getArgs

  when (null allArgs) $ 
    do hPutStrLn stderr "Usage: cabal run Test <test files>"
       exitFailure

  when (allArgs !! 0 == "do") $ 
    let source = allArgs !! 1
        args = map read (words (allArgs !! 2))
        expected = map read (words (allArgs !! 3))
    in if length allArgs <= 3
       then do hPutStrLn stderr "Usage: cabal run Test do <test file> <arguments> <expected result>"
               exitFailure
       else do result <- runTest (source, args, expected)
               if result then exitSuccess else exitFailure

  let (makeJson, testArgs) = partition ("json" ==) allArgs
      (testIdxs, testFiles) = partition (all isDigit) testArgs

  when (null testFiles) $ 
    do hPutStrLn stderr "No tests"
       exitFailure
  
  allTests <- concat <$> mapM readTests testFiles

  let tests | null testIdxs = allTests
            | otherwise = pickTests (ranges testIdxs) allTests
  
  if not (null makeJson) 
  then putStrLn (testsJson tests)
  else do callProcess "cabal" ["build", "Wasm", "--verbose=0"]
          results <- mapM runTest tests
          if and results 
          then do putStrLn (show (length results) ++ " tests passed.")
                  exitSuccess 
          else do putStrLn (show (length $ filter id results) ++ " of " ++ show (length results) ++ " tests passed.")
                  exitFailure

  where ranges [] = []
        ranges (s : ss)
            | Just i <- readMaybe s          = i : ranges ss
            | otherwise                      = error "Unknown ranges"
            where go []           = []
                  go (i : j : ks) = i : j : go ks
      
        pickTests (i : j : ks) allTests = between i j ++ pickTests ks allTests
          where between i j = take (j - i + 1) (drop (i - 1) allTests)
        pickTests [i]          allTests = [allTests !! (i - 1)]
        pickTests []           allTests = []

        testsJson tests = 
            unlines [ "{\"name\" : \"Test " ++ show n ++ "\", \"setup\" : \"\", \"run\" : \"cabal run Test do " ++ s ++ "'" ++ unwords (map show args) ++ "' '" ++ unwords (map show expected) ++ "'\", \"input\" : \"\", \"output\" : \"\", \"comparison\" : \"included\", \"timeout\" : 0.5, \"points\" : 1 },"
                    | (n, (s, args, expected)) <- zip [1..] tests
                    ] 

        readTests :: FilePath -> IO [Test]
        readTests source =
          do ss <- lines <$> readFile source
             ss' <- concat <$> mapM expand ss
             return (map read ss')

        expand :: String -> IO [String]
        expand s@('(' : _) = return [s]
        expand s = do ss <- lines <$> readFile s
                      concat <$> mapM expand ss

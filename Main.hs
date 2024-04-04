module Main where

import Data.Word
import Data.List (intercalate)
import System.Environment
import System.Exit
import System.IO

import Parser
import Syntax
import Justinwork

main :: IO ()
main = do args <- getArgs
          case args of
            source:moreArgs -> do m <- readModule source
                                  let xs = map read moreArgs
                                      is = interp m xs
                                  putStrLn (intercalate " " (map show is))
                                  exitSuccess
            _ -> do hPutStrLn stderr "Usage: Wasm <source> <args>"
                    exitFailure

interp :: Module -> [Word32] -> [Word32]
interp = jlinInterp


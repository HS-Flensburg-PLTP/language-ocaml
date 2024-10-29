module Main (main) where

import qualified Language.OCaml.Parser
import qualified Path
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      filePath <- Path.parseRelFile file
      result <- Language.OCaml.Parser.parseFile filePath
      case result of
        Right ast -> print ast
        Left err -> putStrLn err
    _ -> putStrLn "Usage: ocaml-parser <file>"

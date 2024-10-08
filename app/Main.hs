-- automatically generated by BNF Converter
module Main where

import Data.List (intercalate)
import Language.Ocaml.Lex (Tok (..), TokSymbol (..), Token (..))
import Language.Ocaml.Par (myLexer, pImplementation)
import Language.Ocaml.Print (Print, printTree)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

runFile :: FilePath -> IO ()
runFile f = putStrLn f >> readFile f >>= run

showToken :: Token -> String
showToken (PT _ token) = showTok token
showToken (Err pos) = "lexical error at " ++ show pos
showToken EOF = "EOF"

showTok :: Tok -> String
showTok (TK tokSymbol) = tsText tokSymbol
showTok (TI _) = "TI"
showTok (TV _) = "TV"
showTok (TD _) = "TD"
showTok (TC _) = "TC"
showTok (TL _) = "TL"
showTok (T_STRING _) = "L_STRING"
showTok (T_CHAR _) = "L_CHAR"
showTok (T_FLOAT _) = "L_FLOAT"
showTok (T_RELOP _) = "L_RELOP"
showTok (T_CONCATOP _) = "L_CONCATOP"
showTok (T_PLUSMINUSOP _) = "L_PLUSMINUSOP"
showTok (T_MULTDIVOP _) = "L_MULTDIVOP"
showTok (T_POWOP _) = "L_POWOP"
showTok (T_DOTOP _) = "L_DOTOP"
showTok (T_LETOP _) = "L_LETOP"
showTok (T_ANDOP _) = "L_ANDOP"
showTok (T_DecimalLiteral _) = "L_DecimalLiteral"
showTok (T_DecimalLiteralModifier _) = "L_DecimalLiteralModifier"
showTok (T_HexLiteral _) = "L_HexLiteral"
showTok (T_HexLiteralModifier _) = "L_HexLiteralModifier"
showTok (T_OctLiteral _) = "L_OctLiteral"
showTok (T_OctLiteralModifier _) = "L_OctLiteralModifier"
showTok (T_BinLiteral _) = "L_BinLiteral"
showTok (T_BinLiteralModifier _) = "L_BinLiteralModifier"
showTok (T_LABEL _) = "L_LABEL"
showTok (T_LIDENT _) = "L_LIDENT"
showTok (T_OPTLABEL _) = "L_OPTLABEL"
showTok (T_PREFIXOP _) = "L_PREFIXOP"
showTok (T_HASHOP _) = "L_HASHOP"
showTok (T_QUOTED_STRING_EXPR _) = "L_QUOTED_STRING_EXPR"
showTok (T_QUOTED_STRING_ITEM _) = "L_QUOTED_STRING_ITEM"
showTok (T_UIDENT _) = "L_UIDENT"
showTok (T_EQUAL _) = "L_EQUAL"
showTok (T_BANGEQUAL _) = "L_BANGEQUAL"
showTok (T_PLUSEQ _) = "L_PLUSEQ"
showTok (T_MINUS _) = "L_MINUS"
showTok (T_BANG _) = "L_BANG"
showTok (T_AMPERAMPER _) = "L_AMPERAMPER"
showTok (T_AMPERSAND _) = "L_AMPERSAND"
showTok (T_BARBAR _) = "L_BARBAR"
showTok (T_COLONEQUAL _) = "L_COLONEQUAL"
showTok (T_GREATER _) = "L_GREATER"
showTok (T_LESS _) = "L_LESS"
showTok (T_MINUSDOT _) = "L_MINUSDOT"
showTok (T_OR _) = "L_OR"
showTok (T_PERCENT _) = "L_PERCENT"
showTok (T_PLUS _) = "L_PLUS"
showTok (T_PLUSDOT _) = "L_PLUSDOT"
showTok (T_STAR _) = "L_STAR"

run :: String -> IO ()
run s =
  let ts = myLexer s
   in case pImplementation ts of
        Left s -> do
          putStrLn "\nParse              Failed...\n"
          putStr "Tokens:"
          putStr ("[" ++ intercalate ", " (map (show . showToken) ts) ++ ", \"%eof\" ]")
          putStr (show ts)
          putStrLn s
          exitFailure
        Right tree -> do
          putStrLn "\nParse Successful!"
          putStr ("[" ++ intercalate ", " (map (show . showToken) ts) ++ ", \"%eof\" ]")
          showTree tree
          exitSuccess

showTree :: (Show a, Print a) => a -> IO ()
showTree tree = do
  putStr $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStr $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $
    unlines
      [ "usage: Call with one of the following argument combinations:",
        "  --help          Display this help message.",
        "  (no arguments)  Parse stdin verbosely.",
        "  (files)         Parse content of files verbosely.",
        "  -s (files)      Silent mode. Parse content of files silently."
      ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> getContents >>= run
    "-s" : fs -> mapM_ runFile fs
    fs -> mapM_ runFile fs

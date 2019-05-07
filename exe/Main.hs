module Main(main) where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Environment(getArgs)
import Text.PrettyPrint(vcat)


import Parser.Monad(parse)
import Parser(decls)
import Parser.AST
import Pretty

main :: IO ()
main =
  do [file] <- getArgs
     txt <- Text.readFile file
     case parse decls (Text.pack file) txt of
       Left err -> print err
       Right (_,ds) -> print (vcat (map ppSyn ds))



import System.Environment
import Data.List
import Data.Char

main = do
 putStr "Numero de palavras: "
 args <- getArgs
 txt <- readFile (head args)
 putStr((show(length(words(unlines(lines(txt))))))++"\n")

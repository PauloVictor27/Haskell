import System.Environment
import Data.List
import Data.Char

paragrafos [] = 0
paragrafos (x:xs) = if x == "" then paragrafos xs else 1 + paragrafos xs

main = do
 putStr "Numero de paragrafos: "
 args <- getArgs
 txt <- readFile (head args)
 putStr((show(paragrafos(lines(txt))))++"\n")

import System.Environment
import Data.List
import Data.Char

substituicao [] = []
substituicao (x:xs) = if x == "a" then ["b"] ++ substituicao xs else [x] ++ substituicao xs

main = do
 putStr "Novo arquivo: "
 args <- getArgs
 txt <- readFile (head args)
 putStr((show(substituicao(words(unlines((lines(txt)))))))++"\n")
 --putStr((show((words(txt)))++"\n")

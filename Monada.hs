import Data.List
import Data.Char

linhas [] = []
linhas x = lines x

nlinhas [] = []
nlinhas x = zip [1..] (linhas x)

allwords [] = []
allwords ((x,y):xs) = (zip (repeat x) (words y))++(allwords(xs))
--allwords ((x,y):xs) = map (\w->(x,w))(words y)++allwords xs

documentacao [] = []
documentacao x = allwords(nlinhas(x))

sortls xs = sortBy cmpWord xs
 where cmpWord (_,a) (_,b) = compare a b 

almalgamate [] = []
almalgamate xxs@(x:xs) = (map fst (takeWhile eqw xxs), snd x):almalgamate(dropWhile eqw xs)
 where eqw (_,a) = map toUpper a == map toUpper (snd x)
 
makeindex txt = almalgamate(sortls(documentacao(txt)))

ola = do 
 putStr "Qual o seu nome? "
 nome <- getLine
 putStr ("Ola " ++ nome++"\n")

--ola2 = putStr "Qual o seu nome" >> getLine >>= (\nome->putStr("Ola "++nome))

fat 0 = 1
fat n = n*fat(n-1)

soma [] = 0
soma (x:xs) = x + soma(xs)

dobro n = 2*n

main1 = do
 putStr "Digite n: "
 n <- getLine
 putStr ("Fatorial: "++show(fat(read n))++"\n")

--Ex: E:\Documentos\PAP\Teste.txt
main2 = do
 putStr "Arquivo: "
 arq <- getLine
 txt <- readFile arq
 putStr (txt++"\n")
   
main3 = do
  putStr "Digite a lista: "
  n <- getLine
  putStr ("Somatorio: "++show(soma(read n))++"\n")
  
main4 = do
 putStr "Arquivo: "
 arq <- getLine
 txt <- readFile arq
 let indice = makeindex txt
 putStr ((show indice)++"\n")
 
main5 = do
 putStr "Arquivo: "
 arq <- getLine
 txt <- readFile arq
 let indice = fat(read txt)
 putStr ("Fatorial: "++(show indice)++"\n")
 writeFile arq (show indice)

main6 = do
 putStr "Arquivo: "
 arq <- getLine
 txt <- readFile arq
 let indice = dobro(read txt)
 putStr ("Dobro: "++(show indice)++"\n")
 writeFile arq (show indice)

main7 = do
 putStr "Arquivo: "
 arq <- getLine
 txt <- readFile arq
 let indice = "1"++txt
 putStr ("Incremento: "++indice++"\n")
 writeFile arq indice

import System.Environment
import Data.List
import Data.Char
import qualified Data.Binary.Put as P
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as I

data Huffman = Folha Char Int | No Int (Huffman) (Huffman) 
 deriving Show

{-
	frequenciaSimbolos :: String -> [Huffman]
	construirArvore :: [Huffman] -> Huffman
	codHuffman :: Huffman -> [(Char, String)]
	compactar :: String -> Huffman -> String
	descompacta :: String -> Huffman - String
-}

{-----------------------------
	FUNÇÔES AUXILIARES
-----------------------------}

--Contagem de elementos de uma lista
contagem a [] = 0
contagem a (x:xs) = if a == x then 1 + contagem a xs else contagem a xs

--Remove um elemento de uma lista
remove a [] = []
remove a (x:xs) = if a /= x then [x] ++ remove a xs else remove a xs

--Remove um elemento de uma lista de duplas
removedupla (a,b) [] = []
removedupla (a,b) ((x,y):xs) = if a == x && b == y then removedupla (a,b) (xs) else [(x,y)] ++ removedupla (a,b) (xs)

--Remove os dois primeiros elementos de uma lista de folhas
remove2primeiros ((Folha x y):(Folha xs ys):xss) = xss

--Conta quantas vez um elemento aparece
frequencia [] = []
frequencia (x:xs) = [(x,1 + contagem x xs)] ++ frequencia(remove x xs)

--Retorna o menor elemento de uma lista de duplas
menor [(x,y)] = (x,y)
menor ((x,y):xs) = if y < snd(menor xs) then (x,y) else menor xs

--Ordena uma lista do menor pro maior
ordena [] = []
ordena xs = [x] ++ ordena (removedupla x xs)
 where x = menor xs

--Cria a lista de folhas
listahuff [] = []
listahuff ((x,y):xs) = [Folha x y] ++ listahuff xs

--Cria o primeiro no
primeiroNo ((Folha x y):(Folha xs ys):xss) = No (y + ys) (Folha xs ys) (Folha x y)

--Insere folha no No da arvore (Como a lista já é ordenada, sempre insere em ordem)
insersao (Folha a b) (No x (esq) (dir)) = No (b+x) (Folha a b) (No x (esq) (dir))

--Insere uma lista de folhas em uma arvore
insersao2 [] (No x (esq) (dir)) = (No x (esq) (dir))
insersao2 ((Folha x y):xss) (No a (esq) (dir)) = insersao2 xss (insersao (Folha x y) (No a (esq) (dir)))

--Cria uma lista da arvore ordenada
emOrdem (Folha a b) = [Folha a b]
emOrdem (No x (esq) (dir)) = (emOrdem esq) ++ (emOrdem dir)

--Decimal pra binario
decimal2binario 0 = []
decimal2binario n
 |(mod n 2 == 1) = ["1"] ++ decimal2binario (div n 2)
 |(mod n 2 == 0) = ["0"] ++ decimal2binario (div n 2)
decimal2binario2 n = concat (reverse (decimal2binario n))

--Binario pra decimal
binario2decimal [] = 0
binario2decimal (x:xs) = if x == '1' then 2^(length xs) + binario2decimal xs else binario2decimal xs

binario2decimal2 [] = []
binario2decimal2 ((x:xs):xss) = [binario2decimal (x:xs)] ++ binario2decimal2 xss

--Coloca o numero binario em 8-bits
oitoBits 0 (x:xs) = (x:xs)
oitoBits n (x:xs) = ["0"] ++ oitoBits (n-1) (x:xs)

--Converte o segundo elemento de uma lista de duplas pra binario
binarioDupla [] = []
binarioDupla ((a,b):xs) = [(a, concat (oitoBits (8 - length (decimal2binario2 b))[decimal2binario2 b]))] ++ binarioDupla xs

--Cria uma lista de duplas com o Char da folha com o codigo huffman em decimal
codDecimal ((Folha a b):xs) = zip ((Folha a b):xs) [0..]
codDecimal2 [] = []
codDecimal2 (((Folha a b),c):xs) = [(a,c)] ++ codDecimal2 xs
codDecimal3 ((Folha a b):xs) = codDecimal2 (codDecimal ((Folha a b):xs))
codDecimal4 (x:xs) = codDecimal3 (emOrdem (construirArvore (x:xs)))

--Retorna o primeiro elemento de uma lista de duplas
buscaPrimeiroElemento a ((x,y):xs) = if a == y then x else buscaPrimeiroElemento a xs
buscaPrimeiroElemento2 [] ((x,y):xs) = []
buscaPrimeiroElemento2 (a:as) ((x,y):xs) = [buscaPrimeiroElemento a ((x,y):xs)] ++ buscaPrimeiroElemento2 as ((x,y):xs)

--Retorna o segundo elemento de uma lista de duplas
buscaSegundoElemento a ((x,y):xs) = if a == x then y else buscaSegundoElemento a xs
buscaSegundoElemento2 [] ((x,y):xs) = []
buscaSegundoElemento2 (a:as) ((x,y):xs) = [buscaSegundoElemento a ((x,y):xs)] ++ buscaSegundoElemento2 as ((x,y):xs)

--Quebra uma string em 8 digitos
pega8bits [] = []
pega8bits (x1:x2:x3:x4:x5:x6:x7:x8:xs) = [([x1] ++ [x2] ++ [x3] ++ [x4] ++ [x5] ++ [x6] ++ [x7] ++ [x8])] ++ pega8bits xs

{-----------------------------
	FUNÇÔES PRINCIPAIS
-----------------------------}

--Primeira Função:
frequenciaSimbolos (x:xs) = listahuff(ordena(frequencia((x:xs))))

--Segunda Função:
construirArvore (x:xs) = insersao2 (remove2primeiros (frequenciaSimbolos (x:xs))) (primeiroNo (frequenciaSimbolos (x:xs)))

--Terceira Função:
codHuffman (x:xs) =  codDecimal4 (x:xs)

--Quarta Função:
compactar (x:xs) = concat (buscaSegundoElemento2 (x:xs) (binarioDupla(codDecimal4 (x:xs))))

--Quinta Função
descompactar (a:as) (b:bs)  = buscaPrimeiroElemento2 (binario2decimal2 (pega8bits (a:as))) (b:bs)

--Função Principal
principal = do
 arg <- getLine
 let codigo = codHuffman arg
 let compacto = compactar arg
 putStrLn ("Frequencia de Simbolos: "++(show (frequenciaSimbolos arg)))
 putStrLn ("Arvore de Huffman: "++(show (construirArvore arg)))
 putStrLn ("Codigo de Huffman: "++(show (codigo)))
 putStrLn ("Compactacao: "++(show (compacto)))
 putStrLn ("Descompactar: "++(show (descompactar compacto codigo)))
 
{-----------------------------
	FUNÇÔES DE ARQUIVOS
-----------------------------}

put [] = P.flush
put ((c,f):xs) = do
 P.putWord8 (I.c2w c)
 P.putWord32be (toEnum f)
 put xs

escrita = do
 txt <- readFile "Teste.txt"
 let xs = frequencia txt
 let bs = P.runPut(put xs)
 L.writeFile "Teste.bin" bs
 
getReg = do
 c <- G.getWord8
 f <- G.getWord32be
 return (c, f)
 
getRegs = do
 empty <- G.isEmpty
 if empty then return []
 else do {r <- getReg; rs <- getRegs; return (r:rs);}
 
leitura arq = do
 bs <- L.readFile arq
 let rs = G.runGet getRegs bs
 printRegs rs

printRegs [] = return ()
printRegs (r:rs) = do
 printReg r
 printRegs rs
 
printReg (c, f) = putStrLn ((show(I.w2c c))++"-"++show f)

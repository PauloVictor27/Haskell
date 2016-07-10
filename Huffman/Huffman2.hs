import System.Environment
import Data.List
import Data.Char
import qualified Data.Binary.Put as P
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as I

data Huffman = Folha Char Int | No Int Huffman Huffman 
 deriving Show

{-
	frequenciaSimbolos :: String -> [Huffman]
	construirArvore :: [Huffman] -> Huffman
	codHuffman :: Huffman -> [(Char, String)]
	compactar :: String -> Huffman -> String
	descompacta :: String -> Huffman -> String
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

--Conta quantas vez um elemento aparece
frequencia [] = []
frequencia (x:xs) = (x, length(filter(==x) xs) +1):frequencia(filter(/=x) xs)

--Transforma lista de duplas em lista de folhas
listahuff [] = []
listahuff ((x,y):xs) = [Folha x y] ++ listahuff xs

--Retorna a frequencia de Huffman
frequenciaHuffman (Folha _ f) = f
frequenciaHuffman (No f _ _)= f

--Compara Huffmans
comparaHuffman a b = compare (frequenciaHuffman a) (frequenciaHuffman b)

--Retorna o menor elemento de uma lista de duplas
menor [(x,y)] = (x,y)
menor ((x,y):xs) = if y < snd(menor xs) then (x,y) else menor xs

--Ordena uma lista do menor pro maior
ordena [] = []
ordena xs = [x] ++ ordena (removedupla x xs)
 where x = menor xs

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

--Binario pra decimal usando listas
binario2decimal2 [] = []
binario2decimal2 ((x:xs):xss) = [binario2decimal (x:xs)] ++ binario2decimal2 xss

--Retorna o segundo elemento de uma lista de duplas
buscaSegundoElemento a ((x,y):xs) = if a == x then y else buscaSegundoElemento a xs
buscaSegundoElemento2 [] ((x,y):xs) = []
buscaSegundoElemento2 (a:as) ((x,y):xs) = [buscaSegundoElemento a ((x,y):xs)] ++ buscaSegundoElemento2 as ((x,y):xs)

--Quebra uma string em 8 digitos
pega8bits [] = []
--pega8bits (x1:x2:x3:x4:x5:x6:x7:x8:xs) = [([x1] ++ [x2] ++ [x3] ++ [x4] ++ [x5] ++ [x6] ++ [x7] ++ [x8])] ++ pega8bits xs
pega8bits (x:xs) = [take 8 (x:xs)] ++ pega8bits (drop 8 (x:xs))

{-----------------------------
	FUNÇÔES PRINCIPAIS
-----------------------------}

--Primeira Função:
frequenciaSimbolos (x:xs) = listahuff(ordena(frequencia((x:xs))))

--Segunda Função:
construirArvore (x:xs:xss)= insersao (x:xs:xss) where
 insersao (t:[]) = t
 insersao ((Folha y ys):(Folha t ts):xss) = insersao (sortBy comparaHuffman (No (ys+ts) (Folha t ts) (Folha y ys):xss))
 insersao ((Folha y ys):(No a b c):xss) = insersao (sortBy comparaHuffman (No (ys+a) (No a b c) (Folha y ys):xss))
 insersao ((No a b c):(Folha y ys):xss) = insersao (sortBy comparaHuffman (No (ys+a) (Folha y ys) (No a b c):xss))
 insersao ((No a b c):(No d e f):xss) = insersao (sortBy comparaHuffman (No (a+d) (No d e f) (No a b c):xss))

--Terceira Função:	
codHuffman a (Folha c f) =  [(c,a)]
codHuffman a (No f esq dir) =  (codHuffman (a++"0") esq) ++ (codHuffman (a++"1") dir)

--Quarta Função:
compactar (x:xs) = concat (buscaSegundoElemento2 (x:xs) (codHuffman "" (construirArvore (frequenciaSimbolos (x:xs)))))

--Quinta Função
descompactar x (Folha c f) (No f2 esq2 dir2) = c:(descompactar x (No f2 esq2 dir2) (No f2 esq2 dir2))
descompactar [] (No f1 esq1 dir1) (No f2 esq2 dir2) = []
descompactar (x:xs) (No f1 esq1 dir1) (No f2 esq2 dir2)
 | x == '0' = descompactar (xs) (esq1) (No f2 esq2 dir2)
 | x == '1' = descompactar (xs) (dir1) (No f2 esq2 dir2)

--Função Principal
principal = do
 --putStr ("Digite uma frase: ")
 --arg <- getLine
 arg <- readFile "Teste.txt"
 putStrLn ("String: "++(show (arg)))
 let freq = frequenciaSimbolos arg
 let arvore = construirArvore freq
 let codigo = codHuffman "" arvore
 let compacto = compactar arg
 putStrLn ("Frequencia de Simbolos: "++(show (freq)))
 putStrLn ("Arvore de Huffman: "++(show (arvore)))
 putStrLn ("Codigo de Huffman: "++(show (codigo)))
 putStrLn ("Compactacao: "++(show (compacto)))
 putStrLn ("Descompactar: "++(show (descompactar compacto (arvore) (arvore))))
 
{-----------------------------
	FUNÇÔES DE ARQUIVOS
-----------------------------}

put0 freq = do
 P.putWord32be (toEnum(length freq))

put1 comp = do 
 P.putWord32be (toEnum(length comp))

put2 [] = P.flush 
put2 ((c,f):xs) = do
 P.putWord8 (I.c2w c)
 P.putWord32be (toEnum f)
 put2 xs

put3 [] = P.flush 
put3 (x:xs) = do
 P.putWord8 (I.c2w x)
 put3 xs

escrita = do
 txt <- readFile "Teste.txt"
 let xs = frequencia txt
 let xss = compactar txt
 let bs = P.runPut(put0 xs)
 let bss = P.runPut(put1 xss)
 let bsss = P.runPut(put2 xs)
 let bssss = P.runPut(put3 xss)
 L.writeFile "Teste.bin" bs
 L.appendFile "Teste.bin" bss
 L.appendFile "Teste.bin" bsss
 L.appendFile "Teste.bin" bssss

getReg0 = do
 c <- G.getWord32be
 return c

getReg1 = do
 f <- G.getWord32be
 return f

getReg2 a = do
 c <- G.getWord8
 f <- G.getWord32be
 return (c, f)

getReg3 a = do
 f <- G.getWord8
 return f

{-
getRegs = do
 let xs = getReg0
 let xss = getReg1
 let xsss = getReg2 (xs)
 let xssss = getReg3 (xss)
 return (xs,xss,xsss,xssss)
 
leitura arq = do
 bs <- L.readFile arq
 let (freq,bits,lista,codigo) = G.runGet getRegs bs
 putStrLn ("Codigo: "++(show (descompactar (I.w2c  codigo) (construirArvore(listahuff(ordena(I.w2c lista)))) (construirArvore(listahuff(ordena(I.w2c lista)))))))
-}

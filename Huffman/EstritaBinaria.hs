import qualified Data.Binary.Put as P
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as I
import System.Environment
import Data.List
import Data.Char

--Contagem de elementos de uma lista
contagem a [] = 0
contagem a (x:xs) = if a == x then 1 + contagem a xs else contagem a xs

--Remove um elemento de uma lista
remove a [] = []
remove a (x:xs) = if a /= x then [x] ++ remove a xs else remove a xs

--Conta quantas vez um elemento aparece
frequencia [] = []
frequencia (x:xs) = [(x,1 + contagem x xs)] ++ frequencia(remove x xs)

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

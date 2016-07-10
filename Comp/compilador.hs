--Entrega: 20/06

import Text.ParserCombinators.Parsec
import Data.Char

data Expr = E Expr Expr | Ou Expr Expr | Nao Expr | Bi Expr Expr | Im Expr Expr | Var String
 deriving Show

--GRAMATICA:
{----------------
	A  => BA'
	A' => <->BA'
	   => #
	B  => CB'
	B' => ->CB'
	   => #
	C  => DC'
	C' => &&DC'
	   => #
	D  => ED'
	D' => ||ED'
	   => #
	E  => ~E
	   => variavel
	   => (A)
----------------}

{----------------
FUNÇÔES AUXILIARES
----------------}

extrairVariavel (E dir esq) =  extrairVariavel dir ++ extrairVariavel esq
extrairVariavel (Ou dir esq) =  extrairVariavel dir ++ extrairVariavel esq
extrairVariavel (Bi dir esq) =  extrairVariavel dir ++ extrairVariavel esq
extrairVariavel (Im dir esq) =  extrairVariavel dir ++ extrairVariavel esq
extrairVariavel (Nao a) = extrairVariavel a
extrairVariavel (Var a) = [a]

analizador (E dir esq) b =  (analizador dir b) && (analizador esq b)
analizador (Ou dir esq) b =  (analizador dir b) || (analizador esq b)
analizador (Bi dir esq) b =  (not (analizador dir b) || (analizador esq b)) && (not (analizador esq b) || (analizador dir b))
analizador (Im dir esq) b =  not (analizador dir b) || (analizador esq b)
analizador (Nao a) b = not (analizador a b)
analizador (Var a) b = buscaSegundoElemento a b

solucao a [] = []
solucao a (x:xs) = if analizador a x == True then x:solucao a xs else solucao a xs

--Retorna o segundo elemento de uma lista de duplas
buscaSegundoElemento a ((x,y):xs) = if a == x then y else buscaSegundoElemento a xs

--Elimina elementos repetidos
eliminaRepetidos [] = []
eliminaRepetidos (x:xs) = x:eliminaRepetidos (filter (/=x) xs)

--Converte decimal pra binario
decimal2binario 0 = ["0"]
decimal2binario n
 |(mod n 2 == 1) = ["1"] ++ decimal2binario (div n 2)
 |(mod n 2 == 0) = ["0"] ++ decimal2binario (div n 2)
decimal2binario2 n = concat (reverse (decimal2binario n))

--Variaveis pra teste
variaveis = ["x1", "x2", "x3"]
variaveisBoleanas = [("x1",True), ("x2",True), ("x3",True)]

--Completa os bits de um numero binario
oitoBits 0 (x:xs) = (x:xs)
oitoBits n (x:xs) = "0" ++ oitoBits (n-1) (x:xs)

--remove o primeiro elemento de uma lista de listas
removePrimeiro [] = []
removePrimeiro ((x:xs):xss) = xs:removePrimeiro xss

--Cria a tabela verdade
tabelaVerdade a [] = []
tabelaVerdade a (x:xs) = (oitoBits (a - (length (decimal2binario2 x))) (decimal2binario2 x)):tabelaVerdade a xs
tabelaVerdade2 a (x:xs) = removePrimeiro (tabelaVerdade a (x:xs))

--Converte binario pra boleano
listaBooleana [] = []
listaBooleana (x:xs) = if x == '0' then True:listaBooleana xs else False:listaBooleana xs

listaBooleana2 [] = []
listaBooleana2 ((x:xs):xss) = listaBooleana (x:xs):listaBooleana2 xss

listaBooleana3 (y:ys) [] = []
listaBooleana3 (y:ys) ((x:xs):xss) = (zip (y:ys) (x:xs)):listaBooleana3 (y:ys) xss

tabelaVerdadeExpr a = listaBooleana3 (eliminaRepetidos (extrairVariavel a)) (listaBooleana2 (tabelaVerdade2 ((length (eliminaRepetidos(extrairVariavel a))) + 1) [0..((2^(length (eliminaRepetidos(extrairVariavel a))))-1)]))

{----------------
FUNÇÔES DE TESTE
----------------}

testeTabelaVerdade = tabelaVerdade2 ((length variaveis) + 1) [0..((2^(length variaveis))-1)]

testeListaBooleana = listaBooleana2 testeTabelaVerdade
testeListaBooleana3 = listaBooleana3 variaveis testeListaBooleana

testeAnalizador = analizador (E (Var "X") (Var "Y")) [("X",True), ("Y",True)]

testeSolucao1 = solucao (E (Var "X") (Nao (Var "X"))) (tabelaVerdadeExpr (E (Var "X") (Var "Y")))
testeSolucao2 = solucao (E (Var "X") (Var "Y")) (tabelaVerdadeExpr (E (Var "X") (Var "Y")))

{----------------
PRINCIPAL
----------------}

main = do {putStr "\nExpressao:";
 e <- getLine;
 case avaliarExpr e of
  Left err -> putStr ((show err)++ "\n")
  --Right r  -> putStr ((show r) ++ "\n" ++ (show (extrairVariavel r)) ++ "\n")}
  Right r  -> putStr ("\nExpressao:\n" ++ (show r) ++ "\n\n" ++ "Variaveis:\n" ++(show (eliminaRepetidos(extrairVariavel r))) ++ "\n\n" ++ "Tabela Verdade:\n" ++(show (tabelaVerdadeExpr (r))) ++ "\n\n" ++ "Solucoes:\n" ++ (show (solucao (r) (tabelaVerdadeExpr (r)))) ++ "\n\n")} 

avaliarExpr e = parse primeiro "Erro:" e

ret v1 Nothing = v1
ret v1 (Just (op, v2)) = op v1 v2
	
{----------------  
Especificacao Sintatica
----------------}

primeiro = do {v1 <- segundo;  -- A  => BA'
          e <- primeiro';
          return (ret v1 e)}

primeiro' = do {string "<->"; -- <->BA'
            v1 <- segundo;
		    e <- primeiro';
			return (Just ((Bi), ret v1 e))} 
		<|> return Nothing -- A' => vazio 

segundo = do {v1 <- terceiro; -- B  => CB'
          e <- segundo';
          return (ret v1 e)}
				
segundo' = do {string "->"; -- B' => ->CB'
            v1 <- terceiro;
		    e <- segundo';
			return (Just ((Im), ret v1 e))} 
		<|> return Nothing -- B' => vazio

terceiro = do {v1 <- quarto; -- C  => DC'
          e <- terceiro';
          return (ret v1 e)}
	   
	  
terceiro' = do {string "&&"; -- C' => &&DC'
            v1 <- quarto;
		    e <- terceiro';
			return (Just ((E), ret v1 e))}
		<|> return Nothing -- C' => vazio 

quarto = do {v1 <- quinto;  -- D  => ED'
          e <- quarto';
          return (ret v1 e)}
	  
quarto' = do {string "||"; -- D' => ||ED'
            v1 <- quinto;
		    e <- quarto';
			return (Just ((Ou), ret v1 e))} 
		<|> return Nothing -- D' => vazio 

quinto = do {char '~'; e <- quinto; return (Nao e)}
         <|> do {char '('; e <- primeiro; char ')'; return e}
         <|> do {e <- variavel; return (Var e)} 
	
{---------------- 
Especificacao Lexica
----------------}

variavel = do {x <- letter;
				xs <- many (letter <|> digit);
				return (x:xs);}

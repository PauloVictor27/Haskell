import Text.ParserCombinators.Parsec
import Data.Char

--Gramatica:
{----------------
	A  : BA'
	A' : <->BA'
	   : #
	B  : CB'
	B' : ->CB'
	   : #
	C  : DC'
	C' : ||DC'
	C' : &&DC'
	C' : #
	D  : ~D
	   : v
	   : f
	   : (A)
----------------}

main = do {putStr "\nExpressao:";
          e <- getLine;
		  case avaliarExpr e of
			Left err -> putStr ((show err)++ "\n")
			Right r  -> putStr ((show r) ++ "\n")}

avaliarExpr e = parse expr "Erro:" e

ret v1 Nothing = v1
ret v1 (Just (op, v2)) = op v1 v2	
  
-- Especificacao sintatica

expr = do v1 <- term  -- E -> TE'
          e <- expr'
          return (ret v1 e)
	   
	  
expr' = do {char '+'; -- E' -> +TE'
            v1 <- term;
		    e <- expr';
			return (Just ((+), ret v1 e))} 
		<|>
		do {char '-'; -- E' -> -TE'
            v1 <- term;
		    e <- expr';
			return (Just ((-), ret v1 e))}
		<|> return Nothing -- E' -> vazio 

term = do v1 <- fator -- T -> FT'
          e <- term'
          return (ret v1 e)
		
		
term' = do {char '*'; -- T' -> *FT'
            v1 <- fator;
		    e <- term';
			return (Just ((*), ret v1 e))} 
		<|>
		do {char '/'; -- T' -> /FT'
            v1 <- fator;
		    e <- term';
			return (Just ((/), ret v1 e))}
		<|> return Nothing -- T' -> vazio

fator = num -- F -> numero 
        <|> do {char '('; e <- expr; char ')'; return e} -- F -> (E)

		
-- Especificacao lexica

num = floating <|> decimal 

floating	
	= do {n <- decimal; 
         frac <- fraction;
		 return (n+frac)}

decimal 
    = do digits <- many1 digit;
         let n = foldl (\x d -> 10*x + toInteger (digitToInt d)) 0 digits
         return (fromIntegral n)
	
fraction    = do {char '.';
                 digits <- many1 digit;
                 return (foldr op 0.0 digits)}			
              <|> return 0  
				
				where
                  op d f    = (f + fromIntegral (digitToInt d))/10.0 

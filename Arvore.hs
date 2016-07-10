data Cor = V|P
 deriving Show

data Arvore a = No a Cor (Arvore a) (Arvore a) | Folha
 deriving Show
 
rotacao (No x3 P (No x1 V a (No x2 V b c)) d) = No x2 V (No x1 P a b) (No x3 P c d)
rotacao (No x3 P (No x2 V (No x1 V a b) c) d) = No x2 V (No x1 P a b) (No x3 P c d)
rotacao (No x1 P a (No x2 V b (No x3 V c d))) = No x2 V (No x1 P a b) (No x3 P c d)
rotacao (No x1 P a (No x3 V (No x2 V b c) d)) = No x2 V (No x1 P a b) (No x3 P c d)
rotacao a = a

insere' e Folha = No e V Folha Folha
insere' e a@(No e1 c esq dir)
 | e < e1 = rotacao(No e1 c (insere' e esq) dir)
 | e > e1 = rotacao(No e1 c esq (insere' e dir))
 | e == e1 = a

insere e a = (No e1 P esq dir)
 where (No e1 c esq dir) = insere' e a

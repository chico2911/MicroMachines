{-|
Module : Tarefa1_2017li1g2
Description : Construir Mapas
-}
module Tarefa1_2017li1g2 where

import LI11718

-- | Lista de caminhos para realização de testes do programa.
testesT1 :: [Caminho]
testesT1 = [[Avanca,CurvaDir,Sobe,CurvaDir,Avanca,CurvaDir,Desce,CurvaDir], 
            [Avanca,Avanca,CurvaDir,Avanca,CurvaDir,Avanca,CurvaDir,Avanca,Avanca,Avanca,CurvaEsq,Avanca,CurvaEsq,Avanca,CurvaEsq,Avanca],
            [Avanca,CurvaEsq,Avanca,Avanca,Avanca,CurvaDir,CurvaEsq,CurvaDir,Avanca,CurvaDir,CurvaEsq,Avanca,Sobe,Avanca,CurvaDir,Avanca,CurvaDir,Desce,CurvaEsq,Avanca,CurvaDir,Avanca,CurvaDir,Avanca,CurvaEsq,Avanca,CurvaEsq,Desce,Avanca,CurvaDir,Sobe,Avanca,CurvaDir,CurvaDir]]

-- | Função que constrói um mapa a partir de um caminho.
constroi :: Caminho -> Mapa
constroi c = Mapa(partida c,dirInit) (atualizaTabuleiro c)

{-| Função que recebe uma orientação de uma peça e ao __rodar à Direita__ dá uma nova orientação da peça.

>>> rodaDir Norte 
Este
-}
rodaDir :: Orientacao -> Orientacao
rodaDir orie | orie == Norte = Este
             | orie == Este  = Sul
             | orie == Sul   = Oeste
             | otherwise     = Norte

{-| Função que recebe uma orientação de uma peça e ao __rodar à Esquerda__ dá uma nova orientação da peça.

>>> rodaEsq Norte
Oeste
-}
rodaEsq :: Orientacao -> Orientacao
rodaEsq orie | orie == Norte = Oeste
             | orie == Oeste = Sul
             | orie == Sul   = Este
             | otherwise     = Norte

{-| Função que recebe uma posição e uma orientação e dá uma nova posição, depois de ter movido a peça na orientação indicada.

>>> move (3,2) Norte
(3,1)
-}
move :: Posicao -> Orientacao -> Posicao
move (x, y) posi | posi == Norte = (x,y-1)
                 | posi == Sul   = (x,y+1)
                 | posi == Este  = (x+1,y)
                 | otherwise     = (x-1,y)


{-| Função que cria uma linha, que é uma lista de peças e posições.

>>> replicatex (4,4) (Peca Lava 0) (1,0) 
[(Peca Lava 0,(1,0)),(Peca Lava 0,(2,0)),(Peca Lava 0,(3,0)),(Peca Lava 0,(4,0))]
-}
replicatex :: Dimensao -> Peca -> Posicao -> [(Peca,Posicao)]
replicatex (x,y) peca (a,b)|  x == 0   = []
                           |  x > 0    = (peca,(a,b)) : replicatex (x-1, y) peca (a+1,b)
                           | otherwise = []

{-| Função que cria uma lista de listas de peças e posições com base numa linha, replicando essa linha __de acordo com a dimensão adequada__.


>>> replicatey (2,2) (Peca Lava 0) (1,0)
[[(Peca Lava 0,(1,0)),(Peca Lava 0,(2,0))],[(Peca Lava 0,(1,1)),(Peca Lava 0,(2,1))]]
-}
replicatey :: Dimensao -> Peca -> Posicao -> [[(Peca,Posicao)]]
replicatey (x,y) peca (a,b)  | y == 0    = []
                             | y  > 0    = replicatex (x,y) peca (a,b) : replicatey (x,y-1) peca (a,b+1)
                             | otherwise = []   

{-| Função que cria um __tabuleiro de lava__, de acordo com a dimensão adequada, recorrendo a uma função auxiliar de nome: replicatey, em que todas as peças são do tipo: Peca Lava 0.

>>> tabuleiroLava [CurvaDir,CurvaDir,CurvaDir,CurvaDir]
[[(Peca Lava 0,(0,0)),(Peca Lava 0,(1,0)),(Peca Lava 0,(2,0)),(Peca Lava 0,(3,0))],
[(Peca Lava 0,(0,1)),(Peca Lava 0,(1,1)),(Peca Lava 0,(2,1)),(Peca Lava 0,(3,1))],
[(Peca Lava 0,(0,2)),(Peca Lava 0,(1,2)),(Peca Lava 0,(2,2)),(Peca Lava 0,(3,2))],
[(Peca Lava 0,(0,3)),(Peca Lava 0,(1,3)),(Peca Lava 0,(2,3)),(Peca Lava 0,(3,3))]]
-}
tabuleiroLava :: Caminho -> [[(Peca,Posicao)]]
tabuleiroLava c = replicatey (dimensao c) (Peca Lava 0) (0,0)


{-| Função que transforma um passo, com uma orientação, uma posição e uma altura, numa peça e posição, com base em várias funções auxiliares.

>>> transpassopeca CurvaDir Este (2,2) 0 
(Peca (Curva Este) 0,(2,2))
-}
transpassopeca :: Passo -> Orientacao -> Posicao -> Altura -> (Peca,Posicao)
transpassopeca x k y z | x == CurvaDir = transCurvaDir x k y z 
                       | x == CurvaEsq = transCurvaEsq x k y z
                       | x == Sobe     = transSobe x k y z
                       | x == Desce    = transDesce x k y z
                       | otherwise     = (Peca Recta z , y)

{-| Função auxiliar que transforma um passo, __Desce__, com uma orientação, uma posição e uma altura, numa peça e posição.

>>> transDesce Desce Este (2,2) 0 
(Peca (Rampa Oeste) (-1),(2,2))
-}
transDesce :: Passo -> Orientacao -> Posicao -> Altura -> (Peca,Posicao)
transDesce x k y z = (Peca (Rampa (sobeDesce k)) (z-1) , y)

{-| Função auxiliar que transforma um passo, __Sobe__,  com uma orientação, uma posição e uma altura, numa peça e posição.

>>> transSobe Sobe Este (2,2) 0 
(Peca (Rampa Este) 0,(2,2))
-}
transSobe :: Passo -> Orientacao -> Posicao -> Altura -> (Peca,Posicao)
transSobe x k y z = (Peca (Rampa k) z  , y)

{-| Função auxiliar que transforma um passo, __Curva Direita__, com uma orientação, uma posição e uma altura, numa peça e posição.

>>> transCurvaDir CurvaDir  Este (2,2) 0 
(Peca (Curva Este) 0,(2,2))
-}
transCurvaDir :: Passo -> Orientacao -> Posicao -> Altura -> (Peca,Posicao)
transCurvaDir x k y z = (Peca (Curva (curvaDux k)) z , y)

{-| Função auxiliar que recebe uma orientação e devolve uma orientação, verificando qual é a curva __de acordo com a orientação de entrada__.

>>> curvaDux Sul
Sul
-}
curvaDux :: Orientacao -> Orientacao
curvaDux k | k == Este  = Este
           | k == Oeste = Oeste
           | k == Norte = Norte
           | otherwise  = Sul

{-| Função auxiliar que transforma um passo, __Curva Esquerda__, com uma orientação, uma posição e uma altura, numa peça e posição.

>>> transCurvaEsq CurvaEsq Este (2,2) 0 
(Peca (Curva Sul) 0,(2,2))
-}
transCurvaEsq :: Passo -> Orientacao -> Posicao -> Altura -> (Peca,Posicao)
transCurvaEsq x k y z = (Peca (Curva (curvaEux k)) z , y)

{-| Função auxiliar que recebe uma orientação e devolve uma orientação, verificando qual é a curva __de acordo com a orientação de entrada__.

>>> curvaEux Sul
Oeste
-}
curvaEux :: Orientacao -> Orientacao
curvaEux k | k == Este  = Sul
           | k == Oeste = Norte
           | k == Norte = Este
           | otherwise  = Oeste


{-| Função que define uma rampa dependendo se __sobe__ ou se __desce__.

>>> sobeDesce Oeste
Este
-}
sobeDesce :: Orientacao -> Orientacao
sobeDesce m | m == Norte = Sul
            | m == Oeste = Este
            | m == Este  = Oeste
            | otherwise  = Norte


{-| Função que recebendo uma orientação e um passo, devolve uma orientação, depois de dado um passo e uma orientação inicial.

>>> atualizaOrienta Oeste CurvaEsq
Sul
-}
atualizaOrienta :: Orientacao -> Passo -> Orientacao
atualizaOrienta x y | y == Avanca && x == Norte = Norte
                    | y == Avanca && x == Sul   = Sul
                    | y == Avanca && x == Este  = Este
                    | y == Avanca && x == Oeste = Oeste
                    | y == Sobe   && x == Norte = Norte
                    | y == Sobe   && x == Sul   = Sul
                    | y == Sobe   && x == Este  = Este
                    | y == Sobe   && x == Oeste = Oeste
                    | y == Desce  && x == Norte = Norte
                    | y == Desce  && x == Sul   = Sul
                    | y == Desce  && x == Este  = Este
                    | y == Desce  && x == Oeste = Oeste
                    | y == CurvaEsq = rodaEsq x  
                    | otherwise = rodaDir x

{-| Função que recebendo uma altura e um passo, devolve uma altura, em que a altura final é a altura dada depois de dado um passo e uma determinada altura.

>>> atualizaAltura 1 Sobe
2
-}
atualizaAltura ::  Altura -> Passo -> Altura
atualizaAltura a p | p == Sobe  = a + 1
                   | p == Desce = a - 1 
                   | otherwise  = a


{-| Função que dado um caminho devolve uma lista de peças e posições, traduzindo os passos em peças e posições com recurso a uma função auxiliar.

>>> passospeca [Avanca, CurvaDir, Sobe, CurvaDir, Avanca, CurvaDir, Desce, CurvaDir]
[(Peca Recta 0,(2,1)),(Peca (Curva Este) 0,(3,1)),(Peca (Rampa Sul) 0,(3,2)),(Peca (Curva Sul) 1,(3,3)),
(Peca Recta 1,(2,3)),(Peca (Curva Oeste) 1,(1,3)),(Peca (Rampa Sul) 0,(1,2)),(Peca (Curva Norte) 0,(1,1))]
-}
passospeca :: Caminho -> [(Peca, Posicao)]
passospeca [] = []
passospeca c = passospecaaux c Este (partida c) 0


{-| Função auxiliar de /"passospeca"/, que recebe um caminho, uma orientação, uma posição e uma altura e devolve uma lista de peças e posições, com recurso a várias funções auxiliares.

>>> passospecaaux ([Avanca, CurvaDir, Sobe, CurvaDir, Avanca, CurvaDir, Desce, CurvaDir]) Este (2,1) 0
[(Peca Recta 0,(2,1)),(Peca (Curva Este) 0,(3,1)),(Peca (Rampa Sul) 0,(3,2)),(Peca (Curva Sul) 1,(3,3)),
(Peca Recta 1,(2,3)),(Peca (Curva Oeste) 1,(1,3)),(Peca (Rampa Sul) 0,(1,2)),(Peca (Curva Norte) 0,(1,1))]
-}
passospecaaux :: Caminho -> Orientacao -> Posicao -> Altura -> [(Peca,Posicao)]
passospecaaux [] o p a = []
passospecaaux (h:t) o p a = transpassopeca h o p a : passospecaaux t (atualizaOrienta o h) (move p (atualizaOrienta o h)) (atualizaAltura a h)


{-| Função que recebe um caminho e devolve uma lista de listas de peças e posições, com recurso a várias funções auxiliares, de modo a dar uma lista das listas de peças e posições.

>>> atuaTabuleiro [Avanca, CurvaDir, Sobe, CurvaDir, Avanca, CurvaDir, Desce, CurvaDir]
[[(Peca Lava 0,(0,0)),(Peca Lava 0,(1,0)),(Peca Lava 0,(2,0)),(Peca Lava 0,(3,0)),(Peca Lava 0,(4,0))],
[(Peca Lava 0,(0,1)),(Peca (Curva Norte) 0,(1,1)),(Peca Recta 0,(2,1)),(Peca (Curva Este) 0,(3,1)),(Peca Lava 0,(4,1))],
[(Peca Lava 0,(0,2)),(Peca (Rampa Sul) 0,(1,2)),(Peca Lava 0,(2,2)),(Peca (Rampa Sul) 0,(3,2)),(Peca Lava 0,(4,2))],
[(Peca Lava 0,(0,3)),(Peca (Curva Oeste) 1,(1,3)),(Peca Recta 1,(2,3)),(Peca (Curva Sul) 1,(3,3)),(Peca Lava 0,(4,3))],
[(Peca Lava 0,(0,4)),(Peca Lava 0,(1,4)),(Peca Lava 0,(2,4)),(Peca Lava 0,(3,4)),(Peca Lava 0,(4,4))]]
-}
atuaTabuleiro :: Caminho -> [[(Peca,Posicao)]]
atuaTabuleiro c = atuaTabuleiroAux (tabuleiroLava c) (passospeca c)

{-| Função auxiliar da função /"atuaTabuleiro"/, que recebe uma lista de listas de peças e posições e uma lista de peças e posições, devolvendo uma lista de peças e posições.

Esta função compara duas listas de peças e posições e dá uma lista de listas de peças e posições.
-}
atuaTabuleiroAux :: [[(Peca,Posicao)]] -> [(Peca,Posicao)] -> [[(Peca,Posicao)]]
atuaTabuleiroAux [] l = []
atuaTabuleiroAux l [] = []
atuaTabuleiroAux (h:t) l = [tabuaux h l] ++ atuaTabuleiroAux t l


{-| Função auxiliar da função /"atuaTabuleiroAux"/, que recebe duas listas de peças e posições e devolve uma lista de peças e posições.

>>> tabuaux [(Peca Lava 0,(0,0)),(Peca Lava 0,(1,0)),(Peca Lava 0,(2,0)),(Peca Lava 0,(3,0)),(Peca Lava 0,(4,0))] [(Peca Lava 0,(0,3)),(Peca (Curva Oeste) 1,(1,3)),(Peca Recta 1,(2,3)),(Peca (Curva Sul) 1,(3,3)),(Peca Lava 0,(4,3))]
[(Peca Lava 0,(0,0)),(Peca Lava 0,(1,0)),(Peca Lava 0,(2,0)),(Peca Lava 0,(3,0)),(Peca Lava 0,(4,0))]
-}
tabuaux :: [(Peca,Posicao)] -> [(Peca,Posicao)] -> [(Peca,Posicao)]
tabuaux [] y = []
tabuaux (h:t) (x:xs) = tabauxi h (x:xs) ++ tabuaux t (x:xs)

{-| Função auxiliar da função /"tabuaux"/, que recebe uma lista e uma posição e uma lista de peças e posições, devolvendo uma lista de peças e posições.

Esta função percorre uma lista __x__ comparando com __a__ e resulta em __y__.

>>> tabauxi (Peca Recta 1,(2,3)) [(Peca Lava 0,(0,3)),(Peca (Curva Oeste) 1,(1,3)),(Peca Recta 1,(2,3)),(Peca (Curva Sul) 1,(3,3)),(Peca Lava 0,(4,3))]
[(Peca Recta 1,(2,3))]
-}
tabauxi :: (Peca,Posicao) -> [(Peca,Posicao)] -> [(Peca,Posicao)] 
tabauxi h [] = [h]
tabauxi h (x:xs) = if snd h == snd x
                   then [x]
                   else tabauxi h xs    

{-| Função que recebe um caminho e devolve um Tabuleiro, com recurso a várias funções auxiliares, de modo a atualizar o Tabuleiro.

>>> atualizaTabuleiro  [Avanca, CurvaDir, Sobe, CurvaDir, Avanca, CurvaDir, Desce, CurvaDir]
[[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],
[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],
[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],
[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
-}
atualizaTabuleiro :: Caminho -> Tabuleiro 
atualizaTabuleiro c = tabauxy (atuaTabuleiro c)

{-| Função auxiliar da função /"atualizaTabuleiro"/, que recebe uma lista de listas de peças e posições e devolve um tabuleiro.

Esta função forma um tabuleiro a partir de uma lista de listas de peças e posições.

>>> tabauxy [[(Peca Lava 0,(0,0)),(Peca Lava 0,(1,0)),(Peca Lava 0,(2,0)),(Peca Lava 0,(3,0)),(Peca Lava 0,(4,0))],[(Peca Lava 0,(0,1)),(Peca (Curva Norte) 0,(1,1)),(Peca Recta 0,(2,1)),(Peca (Curva Este) 0,(3,1)),(Peca Lava 0,(4,1))],[(Peca Lava 0,(0,2)),(Peca (Rampa Sul) 0,(1,2)),(Peca Lava 0,(2,2)),(Peca (Rampa Sul) 0,(3,2)),(Peca Lava 0,(4,2))],[(Peca Lava 0,(0,3)),(Peca (Curva Oeste) 1,(1,3)),(Peca Recta 1,(2,3)),(Peca (Curva Sul) 1,(3,3)),(Peca Lava 0,(4,3))],[(Peca Lava 0,(0,4)),(Peca Lava 0,(1,4)),(Peca Lava 0,(2,4)),(Peca Lava 0,(3,4)),(Peca Lava 0,(4,4))]]
[[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
 [Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],
 [Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],
 [Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],
 [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
-}
tabauxy :: [[(Peca,Posicao)]] -> Tabuleiro
tabauxy t = foldr (\ h -> (++) [tabauxz (h)]) [] t

{-| Função auxiliar da função /"tabauxy"/, que recebe uma lista de peças e posições e devolve uma peça.

>>> tabauxz [(Peca Lava 0,(0,3)),(Peca (Curva Oeste) 1,(1,3)),(Peca Recta 1,(2,3)),(Peca (Curva Sul) 1,(3,3)),(Peca Lava 0,(4,3))]
[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0]
-}
tabauxz :: [(Peca,Posicao)] -> [Peca]
tabauxz t = map fst t 

{-|
Module : Tarefa2_2017li1g2
Description : Validar Mapas
-}
module Tarefa2_2017li1g2 where

import LI11718 
import Data.List

-- | Lista de um Tabuleiro para realização de testes do programa.
testesT2 :: [Tabuleiro]
testesT2 = [t1,t2]

-- | Tabuleiro para servir de teste em "testesT2"
t1 :: Tabuleiro
t1 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

-- | Tabuleiro para servir de teste em "testesT2"
t2 :: Tabuleiro
t2 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

-- | Função que dado uma mapa devolve um Bool, dizendo se o mapa é __válido__, /True/, ou __inválido__, /False/.
valida :: Mapa -> Bool
valida m = lavaRetanguloi m && alturaLav m && orieInicialFinal m && validaPosInit m && validaLava m && validaAltura m && linhasColunas m

{-| Função que recebe um mapa e devolve um Bool.

Esta função é usada para verificar se todas as linhas têm o mesmo número de peças.

>>> linhasColunas (Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]])
True
-}
linhasColunas :: Mapa -> Bool
linhasColunas (Mapa p []) = False
linhasColunas (Mapa p t) = linhasColunasX t 

{-| Função auxiliar da função /"linhasColunas"/, que recebe um tabuleiro e devolve um Bool. 

>>> linhasColunasX [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
True
-}
linhasColunasX :: Tabuleiro -> Bool 
linhasColunasX [t] = True
linhasColunasX (h:t) | length h == length (head t) = linhasColunasX t
                     | otherwise = False 

{-| Função que recebe um mapa e devolve uma peça.

Esta função é usada para calcular a peça de partida de um determinado mapa.

>>> posicaoMTabuleiro (Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]])
Peca Recta 0

-}
posicaoMTabuleiro :: Mapa -> Peca
posicaoMTabuleiro (Mapa p (h:t)) = (!!) ((!!) (h:t) (snd(fst p))) (fst(fst p))

{-| Função que recebe uma posição e um tabuleiro e recebe uma peça. 

Esta função é utilizada para dizer qual a peça que esta numa determinada posição no tabuleiro.

>>> posicaoTabuleiro (3,4) ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]])
Peca Lava 0
-}

posicaoTabuleiro :: Posicao -> Tabuleiro -> Peca
posicaoTabuleiro (x,y) h = (!!) ((!!) h y) x


{-| Função que dado um mapa devolve um Bool.

Esta função é usada para ver se a primeira linha, a primeira coluna, a última linha e a última coluna é __tudo lava__.

>>> lavaRetanguloi (Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]])
True
-}
lavaRetanguloi :: Mapa -> Bool
lavaRetanguloi (Mapa p []) = False
lavaRetanguloi (Mapa p t) = lavaRetangulo t

{-| Função auxiliar da função /"lavaRetanguloi"/, que recebe um Tabuleiro e devolve um Bool.

>>> lavaRetangulo ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]])
True
-}
lavaRetangulo :: Tabuleiro -> Bool
lavaRetangulo [] = False
lavaRetangulo [[Peca Lava 0]] = True
lavaRetangulo (h:t) = lavaRetanguloAux h && lavaRetanguloAux (last (h:t)) && lavaRetanguloAuxi (h:t)

{-| Função auxiliar da função /"lavaRetangulo"/, que recebe uma lista de peças e devolve um Bool.

>>> lavaRetanguloAux [Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0]
False
-}
lavaRetanguloAux :: [Peca] -> Bool
lavaRetanguloAux [] = False
lavaRetanguloAux [Peca Lava 0] = True
lavaRetanguloAux (h:t) = h == Peca Lava 0 && lavaRetanguloAux t 

{-| Função auxiliar da função /"lavaRetanguloAux"/, que recebe um Tabuleiro e devolve um Bool.

>>> lavaRetanguloAuxi ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]])
True
-}
lavaRetanguloAuxi :: Tabuleiro -> Bool
lavaRetanguloAuxi [] = True
lavaRetanguloAuxi [[Peca Lava 0]] = True
lavaRetanguloAuxi (h:t) = aux h && lavaRetanguloAuxi t
            where 
                aux :: [Peca] -> Bool
                aux h = head h == Peca Lava 0 && last h == Peca Lava 0


{-| Função que recebe um mapa e devolve um Bool.

Esta função é usada para verificar se a altura da lava é sempre __0__.

>>> alturaLav (Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]])
True
-}
alturaLav :: Mapa -> Bool
alturaLav (Mapa p []) = False
alturaLav (Mapa p t)  = alturaLava t

{-| Função auxiliar da função /"alturaLav"/, que recebe um Tabuleiro e devolve um Bool.

Esta função utiliza as peças do tabuleiro para verificar se as peças lava estão todas a altura 0.

>>> alturaLava ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]])
True
-}
alturaLava :: Tabuleiro -> Bool
alturaLava [] = True
alturaLava [[Peca Lava 0]] = True
alturaLava (h:t) = alturaLavaAux h && alturaLava t

{-| Função auxiliar da função /"alturaLava"/, que recebe uma listas de peças e devolve um Bool. 

>>> alturaLavaAux ([Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0])
True
-}
alturaLavaAux :: [Peca] -> Bool
alturaLavaAux [Peca Lava 0] = True
alturaLavaAux (Peca t a:s) | t == Lava  = altaux a && alturaLavaAux s
                             | otherwise = alturaLavaAux s                          

{-| Função auxiliar da função /"alturaLavaAux"/, que recebe uma altura e devove um Bool.

Esta função vê se todas as peças estão à altura 0.

>>> altaux 0
True

>>> altaux 1
False
-}
altaux :: Altura -> Bool
altaux x | x == 0 = True
         | otherwise = False

{-| Função que dada uma lista, de uma posição e um orientação, devolve uma posição.

Esta função vê qual a posição da peça que é adjacente a uma certa peça, consoante uma orientação.

>>> vizinho ((1,4), Sul)
(1,3)
-}
vizinho :: (Posicao,Orientacao) -> Posicao
vizinho ((x,y),o) | o == Este  = (x-1,y)
                  | o == Oeste = (x+1,y)
                  | o == Norte = (x,y+1)
                  | otherwise   = (x,y-1)

{-| Função que dado um mapa devolve um Bool.

Esta função é usada para ver se a orientação final é igual à orientação inicial.

>>> orieInicialFinal (Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]])
True

-}
orieInicialFinal :: Mapa -> Bool 
orieInicialFinal (Mapa p []) = False
orieInicialFinal (Mapa p t) = orieInicialFinalAux p t

{-| Função auxiliar da função /"orieInicialFinal"/, que dada uma lista, de posições e orientações, e um Tabuleiro devolve um Bool.

>>> orieInicialFinalAux ((1,4),Oeste) ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]])
False
-}
orieInicialFinalAux :: (Posicao,Orientacao) -> Tabuleiro -> Bool
orieInicialFinalAux p [] = False
orieInicialFinalAux p t | isVizinhoPos (vizinho p) t = aux (posicaoTabuleiro (vizinho p) t) (posicaoTabuleiro (fst p) t) p
                        | otherwise = False
    where aux :: Peca -> Peca -> (Posicao,Orientacao) -> Bool
          aux (Peca t a) (Peca t2 a2)  (x,y) | y == Este  && t == Recta                           && a == a2   = True  
                                             | y == Este  && t == Curva Norte || t == Curva Oeste && a == a2   = True
                                             | y == Este  && t == Rampa Este                      && a == a2-1 = True
                                             | y == Oeste && t == Recta                                        = True
                                             | y == Oeste && t == Curva Sul   || t == Curva Este  && a == a2   = True
                                             | y == Oeste && t == Rampa Oeste                     && a == a2-1 = True
                                             | y == Norte && t == Recta                                        = True
                                             | y == Norte && t == Curva Oeste || t == Curva Sul   && a == a2   = True
                                             | y == Norte && t == Rampa Norte                     && a == a2-1 = True
                                             | y == Sul   && t == Recta                                        = True
                                             | y == Sul   && t == Curva Este  || t == Curva Norte && a == a2   = True
                                             | y == Sul   && t == Rampa Sul                       && a == a2-1 = True
                                             | y == Oeste && t == Rampa Este                      && a == a2   = True
                                             | y == Este  && t == Rampa Oeste                     && a == a2   = True
                                             | y == Norte && t == Rampa Sul                       && a == a2   = True
                                             | y == Sul   && t == Rampa Norte                     && a == a2   = True
                                             | otherwise                                                       = False



{-| Função que recebe uma posição e devolve um Bool.

Esta função vê se uma peça numa determinada posição é sua vizinha.

>>> isVizinhoPos (2,1)
True
-}
isVizinhoPos :: Posicao -> Tabuleiro -> Bool
isVizinhoPos (x,y) (h:t) = 0 < x && x < length h && 0 < y && y < length t

{-| Função que recebe um mapa e devolve um Bool.

Esta função verifica se a posição inicial é válida.

>>> validaPosInit (Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]])
True
-}
validaPosInit :: Mapa -> Bool
validaPosInit (Mapa p []) = False
validaPosInit m  = validaPosInitAux (posicaoMTabuleiro m) m

{-| Função auxiliar da função /"validaPosInit"/, que recebe uma peça e um mapa e devolve um Bool.

Esta função vê se uma determinada peça pertence ao mapa.

>>> validaPosInitAux (Peca Recta 0) (Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]])
True
-}
validaPosInitAux :: Peca -> Mapa -> Bool
validaPosInitAux p (Mapa x []) = False
validaPosInitAux (Peca c a) (Mapa (p,o) t) | c == Recta                                              = True
                                           | o == Este  && c  == Curva Sul   || c ==(Curva Este)     = True
                                           | o == Este  && c  == Rampa Este                          = True
                                           | o == Oeste && c  == Curva Norte || c ==(Curva Oeste)    = True
                                           | o == Oeste && c  == Rampa Oeste                         = True
                                           | o == Sul   && c  == Curva Oeste || c ==(Curva Sul)      = True
                                           | o == Sul   && c  == Rampa Sul                           = True
                                           | o == Norte && c  == Curva Este  || c ==(Curva Norte)    = True
                                           | o == Norte && c  == Rampa Norte                         = True
                                           | otherwise                                               = False 

{-| Função que recebe um mapa e devolve uma lista de posições e orientações.

Esta função vê se a peça final do percurso está ligada à peça inicial do percurso.

>>> percurso (Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]])
[((2,1),Este),((3,1),Este),((3,2),Sul),((3,3),Sul),((2,3),Oeste),((1,3),Oeste),((1,2),Norte),((1,1),Norte)]
-}
percurso :: Mapa -> [(Posicao,Orientacao)]  
percurso (Mapa p []) = []
percurso (Mapa p t) = percursoaux p (vizinho p) t

{-| Função auxiliar da função /"percurso"/, que recebe uma posição e uma orientação, uma posição e um tabuleiro e devolve uma lista de posições e orientações.

>>> percursoaux ((2,1),Este) (3,2) ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]])
[((2,1),Este),((3,1),Este),((3,2),Sul)]
-}
percursoaux :: (Posicao,Orientacao) -> Posicao -> Tabuleiro -> [(Posicao,Orientacao)]
percursoaux o (x,y) [] = []
percursoaux o (x,y) t  | o == ((x,y),snd o) = [((x,y),snd o)]
                       | isLava (posicaoTabuleiro (fst o) t) = []
                       | otherwise        = o : percursoaux (posicaon o (x,y) (posicaoTabuleiro (fst o) t)) (x,y) t
               where isLava :: Peca -> Bool
                     isLava (Peca t a) = t == Lava

{-| Função que recebe uma posição e uma orientação, uma posição e uma peça e devolve uma posição e uma orientação.

Esta função vê a posição e a orientação de uma peça, dada a posição e a orientação da peça anterior.

>>> posicaon ((3,2),Sul) (2,1) (Peca Recta 0)
((3,3),Sul)
-}
posicaon :: (Posicao,Orientacao) -> Posicao -> Peca -> (Posicao,Orientacao)
posicaon ((x,y),z) (b,d) (Peca t a) | t == Recta        && z == Este      = ((x+1,y), Este)
                                    | t == Recta        && z == Oeste     = ((x-1,y), Oeste)
                                    | t == Recta        && z == Norte     = ((x,y-1), Norte)
                                    | t == Recta        && z == Sul       = ((x,y+1), Sul)
                                    | t == Curva Norte  && z == Norte     = ((x+1,y), Este)
                                    | t == Curva Sul    && z == Sul       = ((x-1,y), Oeste)
                                    | t == Curva Este   && z == Este      = ((x,y+1), Sul)
                                    | t == Curva Oeste  && z == Oeste     = ((x,y-1), Norte)
                                    | t == Curva Norte  && z == Oeste     = ((x,y+1), Sul)
                                    | t == Curva Sul    && z == Este      = ((x,y-1), Norte)
                                    | t == Curva Este   && z == Norte     = ((x-1,y), Oeste)
                                    | t == Curva Oeste  && z == Sul       = ((x+1,y), Este)
                                    | t == Rampa Norte  && z == Norte     = ((x,y-1), Norte)
                                    | t == Rampa Norte  && z == Sul       = ((x,y+1), Sul)
                                    | t == Rampa Sul    && z == Norte     = ((x,y-1), Norte)
                                    | t == Rampa Sul    && z == Sul       = ((x,y+1), Sul)
                                    | t == Rampa Oeste  && z == Oeste     = ((x-1,y), Oeste)
                                    | t == Rampa Oeste  && z == Este      = ((x,y-1), Este)
                                    | t == Rampa Este   && z == Oeste     = ((x-1,y), Oeste)
                                    | t == Rampa Este   && z == Este      = ((x+1,y), Este)
                                    | otherwise                             = ((b,d),z)

{-| Função recebe um mapa e devolve um Int.

Esta função conta o numero de peças lava que exitem.

>>> contaLava (Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]])
17
-}
contaLava :: Mapa -> Int
contaLava (Mapa p [])    = 0     
contaLava (Mapa p (h:t)) = contaLavaX (Peca Lava 0) h + contaLava (Mapa p t) 

{-| Função auxiliar da função /"contaLava"/, que recebe uma peça e uma lista de peças e devolve um Int.

>>> contaLavaX (Peca (Curva Norte) 0) ([Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0])
0
-}
contaLavaX ::  Peca -> [Peca] -> Int
contaLavaX p [] = 0
contaLavaX p (h:t) | p == h    = 1 + contaLavaX p t
                   | otherwise = contaLavaX p t

{-| Função que recebe um mapa e devolve um Bool.

Esta função soma o número de peças lava e o número de peças do percurso e vê se é igual ao número de peças do tabuleiro. Se o resultado for True, o percurso é válido, senão é inválido.

>>> validaLava (Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]])
True
-}
validaLava :: Mapa -> Bool
validaLava (Mapa p t) | contaLava (Mapa p t) + length (nub (percursoPos (percurso (Mapa p t)))) == tamanhoTabuleiro (Mapa p t) = True
                      | otherwise                                                                                              = False

{-| Função que recebe um mapa e devolve um Int.

Esta função é usada para saber o número de peças existentes num tabuleiro.

>>> tamanhoTabuleiro (Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]])
25
-}
tamanhoTabuleiro :: Mapa -> Int
tamanhoTabuleiro (Mapa p []) = 0 
tamanhoTabuleiro (Mapa p (h:t)) = length h + tamanhoTabuleiro (Mapa p t)

{-| Função que recebe um mapa e devolve um Bool.

Esta função é usada para ver se todas as peças que estão ligadas se encontram à mesma altura.

>>> validaAltura (Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]])
True
-}
validaAltura :: Mapa -> Bool
validaAltura (Mapa m []) = True
validaAltura (Mapa m t)  = validaAlturaAux (percurso (Mapa m t)) t

{-| Função auxiliar da função /"validaAltura"/, que recebe uma lista de posições e orientações e um tabuleiro e devolve um Bool.

>>> validaAlturaAux ([((2,1),Este),((3,1),Este),((3,2),Sul)]) ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]])
True
-}
validaAlturaAux :: [(Posicao,Orientacao)] -> Tabuleiro -> Bool
validaAlturaAux [x] t =True
validaAlturaAux [] t = True
validaAlturaAux (((x,y),o):((xs,ys),os):s) (h:t) = validaAlturaAuxi (posicaoTabuleiro (x,y) (h:t)) o os (posicaoTabuleiro (xs,ys) (h:t)) && validaAlturaAux (((xs,ys),os):s) (h:t)

{-| Função auxiliar da função /"validaAlturaAux"/, que recebe uma peça, uma orientação, uma orientação e uma peça e devolve um Bool.

>>> validaAlturaAuxi (Peca (Curva Norte) 0) Este Oeste (Peca (Curva Este) 0)
True
-}
validaAlturaAuxi :: Peca -> Orientacao -> Orientacao -> Peca -> Bool
validaAlturaAuxi (Peca t a) o os (Peca t1 a1) | t  == Recta        && a  == a1                       = True
                                              | t  == Curva Norte  && a  == a1                       = True
                                              | t  == Curva Sul    && a  == a1                       = True
                                              | t  == Curva Este   && a  == a1                       = True
                                              | t  == Curva Oeste  && a  == a1                       = True
                                              | t  == Curva Oeste  && a  == a1                       = True
                                              | t  == Rampa Norte  && o  == Norte && (a+1) == a1     = True
                                              | t  == Rampa Norte  && o  == Sul && a == a1           = True
                                              | t  == Rampa Oeste  && o  == Oeste && (a+1) == a1     = True
                                              | t  == Rampa Oeste  && o  == Este && a == a1          = True
                                              | t  == Rampa Este   && o  == Este && (a+1) == a1      = True
                                              | t  == Rampa Este   && o  == Oeste && a == a1         = True
                                              | t  == Rampa Sul    && o  == Sul && (a+1) == a1       = True
                                              | t  == Rampa Sul    && o  == Norte && a == a1         = True
                                              | t1 == Rampa Norte  && os == Norte && a == a1         = True
                                              | t1 == Rampa Norte  && os == Sul && a == a1+1         = True
                                              | t1 == Rampa Oeste  && os == Oeste && a == a1         = True
                                              | t1 == Rampa Oeste  && os == Este && a == a1 +1       = True
                                              | t1 == Rampa Este   && os == Este && a == a1          = True
                                              | t1 == Rampa Este   && os == Oeste && a == a1+1       = True
                                              | t1 == Rampa Sul    && os == Sul && a == a1           = True
                                              | t1 == Rampa Sul    && os == Norte && a == (a1+1)     = True 
                                              | otherwise                                            = False

{-| Função que recebe uma lista de posições e orientações e devolve uma lista de posições.

Esta função faz uma lista apenas com as posições do percurso.

>>> percursoPos ([((2,1),Este),((3,1),Este),((3,2),Sul)])
[(2,1),(3,1),(3,2)]
-}
percursoPos :: [(Posicao,Orientacao)] -> [Posicao]
percursoPos t = map fst t
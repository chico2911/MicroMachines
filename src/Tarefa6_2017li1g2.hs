{-|
Module      : Tarefa6_2017li1g2
Description : Módulo da Tarefa 6 para LI1 17/18

Módulo para a realização da Tarefa 6 de LI1 em 2017/18.
-}
module Tarefa6_2017li1g2 where

import LI11718
import Tarefa4_2017li1g2
{-|
Função usada para simular um /bot/ no jogo /Micro Machines/.
Em cada instante, dado o tempo decorrido, o estado do jogo
e o identificador do jogador, toma uma ação.
-}
bot :: Tempo  -- ^ tempo decorrido desde a última decisão
    -> Jogo   -- ^ estado atual do jogo
    -> Int    -- ^ identificador do jogador dentro do estado
    -> Acao   -- ^ a decisão tomada pelo /bot/
bot tick j i = move (mapa j) j tick i

move :: Mapa -> Jogo -> Tempo -> Int -> Acao
move (Mapa e ta) j t i = moveA ta i ((!!) (carros j) i)


moveA :: Tabuleiro -> Int -> Carro -> Acao
moveA [] i c  = error "Tabuleiro vazio"
moveA t i c = movimenteAux (verificaPeca t c) i c 
      where movimenteAux :: Peca -> Int -> Carro -> Acao
            movimenteAux (Peca t1 a) i (Carro p d (x,y)) | t1 == Recta                                      = Acao True False False False (Just i)
                                                         | t1 == Rampa Este    && dir (mytg (x,y)) == Este  = Acao True False False False Nothing
                                                         | t1 == Rampa Este    && dir (mytg (x,y)) == Oeste = Acao False True False False Nothing
                                                         | t1 == Rampa Norte   && dir (mytg (x,y)) ==  Sul  = Acao False True False False Nothing
                                                         | t1 == Rampa Norte   && dir (mytg (x,y)) == Norte = Acao True False False False Nothing
                                                         | t1 == Rampa Sul     && dir (mytg (x,y)) == Norte = Acao False True False False Nothing
                                                         | t1 == Rampa Sul     && dir (mytg (x,y)) == Sul   = Acao True False False False Nothing
                                                         | t1 == Rampa Oeste   && dir (mytg (x,y)) == Este  = Acao False True False False Nothing
                                                         | t1 == Rampa Oeste   && dir (mytg (x,y)) == Oeste = Acao True False False False Nothing
                                                         | t1 == (Curva Este)  && dir (mytg (x,y)) == Este  = Acao True False False True Nothing
                                                         | t1 == (Curva Este)  && dir (mytg (x,y)) == Norte = Acao True False True False Nothing
                                                         | t1 == (Curva Oeste) && dir (mytg (x,y)) == Oeste = Acao True False False True Nothing
                                                         | t1 == (Curva Oeste) && dir (mytg (x,y)) == Sul   = Acao True False True False Nothing
                                                         | t1 == (Curva Sul)   && dir (mytg (x,y)) == Sul   = Acao True False False True Nothing
                                                         | t1 == (Curva Sul)   && dir (mytg (x,y)) == Este  = Acao True False True False Nothing
                                                         | (x,y) == (0,0)                                   = Acao True False False False Nothing



dir :: Angulo -> Orientacao
dir a | cos (angRad a) > cos (pi/4) && sin (angRad a) < sin (pi/4) && sin (angRad a) > sin (-pi/4) = Este
      | cos (angRad a) <= cos (-pi/4) && cos (angRad a) >= cos (-3*pi/4) && sin (angRad a) <= sin (-pi/4)  = Sul
      | cos (angRad a) < cos (-3*pi/4) && sin (angRad a) < sin (3*pi/4) && sin (angRad a) > sin (-3*pi/4) = Oeste
      |otherwise = Norte


verificaPeca :: Tabuleiro -> Carro -> Peca
verificaPeca t (Carro (x,y) a (vx,vy)) = (!!) ((!!) t (truncate y)) (truncate x)


    
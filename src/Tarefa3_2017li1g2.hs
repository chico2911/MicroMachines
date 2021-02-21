{-|
Module : Tarefa3_2017li1g2
Description : Movimentar o Carro
-}
module Tarefa3_2017li1g2 where

import LI11718
import Tarefa2_2017li1g2


-- | Tuplo para a realização de testes do programa.
testesT3 :: [(Tabuleiro,Tempo,Carro)]
testesT3 = [c1,c2,a1,a2,a3,a4]

c1 :: (Tabuleiro,Tempo,Carro)
c1 = (([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]),2,(Carro {posicao = (2.5,1.5), direcao = 45, velocidade = (1,0)}))

c2 :: (Tabuleiro,Tempo,Carro)
c2 =(([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]),2,(Carro {posicao = (2.5,1.5), direcao = 45, velocidade = (1,0)}))
{-| Função que recebe um Tabuleiro, um tempo e um carro e devolve um Maybe Carro.

Esta função diz se o carro é destruído ou não, depois de se movimentar.

-}
movimenta :: Tabuleiro -> Tempo -> Carro -> Maybe Carro
movimenta [] t c = Nothing
movimenta m 0 c = Just c
movimenta m t (Carro { posicao = (x,y), direcao = a, velocidade = (vx,vy)}) = movimentaAux (t/200) 0 (vx,vy) m ( Carro { posicao = (x,y), direcao = a, velocidade = (vx,vy)})
--movimentaAux (t/60) 0 (vx,vy) m 

{-| Função auxiliar da função /"movimenta"/, que recebe um tempo, uma velocidade, um tabuleiro e um carro e devolve um Maybe Carro.

Esta função verifica se o carro é destruído de acordo com a velocidade e a orientação numa determinada peça do tabuleiro.

>>> movimentaAux 2 (1.2,0.8) ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]) (Carro {posicao = (2.1,1.5), direcao = 45, velocidade = (0.5,0.8)})
Nothing
-}
movimentaAux :: Tempo -> Int -> Velocidade -> Tabuleiro -> Carro -> Maybe Carro
movimentaAux t i (vx, vy) m (Carro { posicao = (x,y), direcao = a, velocidade = (vs,vz)}) | i == 200 = verificaPeca t (posicaoTabuleiro (truncate x,truncate y) m) (posicaoTabuleiro (truncate (x+t*vx),truncate (y+t*vy)) m) (truncate (x+t*vx),truncate (y+t*vy)) (Carro { posicao = (x,y), direcao = a, velocidade = (vs,vz)})
                                                                                          | justNothing (verificaCarro) && i >= 0 = movimentaAux t (i+1) (vx, vy) m (Carro { posicao = (atualizaPosJust verificaCarro), direcao = a, velocidade = (vs,vz)}) 
                                                                                          | justNothing (verificaCarro) == False = Nothing

                                                                                        where verificaCarro = verificaPeca t (posicaoTabuleiro (truncate x,truncate y) m) (posicaoTabuleiro (truncate (x+t*vx),truncate (y+t*vy)) m) (truncate (x+t*vx),truncate (y+t*vy)) (Carro { posicao = (x,y), direcao = a,  velocidade = (vs,vz)})

{-Função que recebendo um Maybe Carro, extrai a posição dele.-}
atualizaPosJust :: Maybe Carro -> Ponto
atualizaPosJust (Just Carro { posicao = (x,y), direcao = a, velocidade = (vs,vz)}) = (x,y)


{-Função que recebendo um Maybe Carro devolve um bool. Para que possamos saber se o carro está ou não destruído-}
justNothing :: Maybe Carro -> Bool
justNothing (Just c) = True
justNothing Nothing = False

{-| Função que recebe um tempo, uma peça, uma peça, uma posição e um carro e devolve um Maybe Carro.

Esta função verifica o tipo de peça e o que acontece ao carro nessa determinada peça, de acordo com o seu ponto, orientação e velocidade.

>>> verificaPeca 2 (Peca Recta 0) (Peca (Curva Sul) 0) (2,1) (Carro {posicao = (2.1,1.5), direcao = 45, velocidade = (0.5,0.8)})
Just (Carro {posicao = (3.1,3.1), direcao = 45.0, velocidade = (0.5,0.8)})
-}
verificaPeca :: Tempo -> Peca -> Peca -> Posicao -> Carro -> Maybe Carro
verificaPeca t (Peca t1 a) (Peca t2 a2) x (Carro { posicao = (b,d), direcao = z, velocidade = (vx,vy)})  | (t2 == Lava)           && (a == a2)   = Nothing
                                                                                                         | (t2 == Lava)           && a > a      = Nothing
                                                                                                         | trueRampa (Peca t1 a)  && a2 < a      = ricochete t (Carro { posicao = (b,d), direcao = z, velocidade = (vx,vy)})
                                                                                                         | trueRampa (Peca t1 a)  && (a-a2) < 1 = ricochete t (Carro { posicao = (b,d), direcao = z, velocidade = (vx,vy)})
                                                                                                         | abs(a - a2) > 1                       = Nothing
                                                                                                         | trueRecta (Peca t1 a)  && a == a2     = Just (Carro { posicao = (b+t*vx,d+t*vy), direcao = z, velocidade = (vx,vy)}) 
                                                                                                         | trueCurva (Peca t2 a2) && (a == a2)   = movimentaC (b,d) x (Peca t2 a2) (Carro { posicao = (b+t*vx,d+t*vy), direcao = z, velocidade = (vx,vy)})
                                                                                                         | trueCurva (Peca t2 a2) && a < 0       = richocheteCurva t (Carro { posicao = (b,d), direcao = z, velocidade = (vx,vy)})
                                                                                                         | otherwise                             = ricochete t (Carro { posicao = (b,d), direcao = z, velocidade = (vx,vy)})

{-| Função que recebe uma peça e devolve um Bool.

Esta função verifica se uma peça é do tipo __rampa__.

>>> trueRampa (Peca Recta 0)
False
-}
trueRampa :: Peca -> Bool
trueRampa (Peca t a) | t == Rampa Norte || t == Rampa Sul || t == Rampa Este || t == Rampa Oeste = True
                     | otherwise                                                                 = False

{-| Função que recebe uma peça e devolve um Bool.

Esta função verifica se uma peça é do tipo __curva__.

>>> trueCurva (Peca (Curva Norte) 0)
True
-}
trueCurva :: Peca -> Bool
trueCurva (Peca t a) | t == Curva Norte || t == Curva Sul || t == Curva Este || t == Curva Oeste = True
                     | otherwise                                                                 = False

{-| Função que recebe uma peça e devolve um Bool.

Esta função verifica se uma peça é do tipo __recta__.

>>> trueRecta (Peca (Rampa Sul) 0)
False
-}
trueRecta :: Peca -> Bool
trueRecta (Peca t a) = t == Recta

{-| Função que recebe um ponto, uma posição, uma peça e um carro e devolve um Bool.

Esta função faz com que o carro se movimente pelo percurso.

>>> movimentaC (2.5,1.5) (2,1) (Peca (Curva Norte) 0) (Carro {posicao = (2.1,1.5), direcao = 45, velocidade = (0.5,0.8)})
Nothing
-}
movimentaC :: Ponto -> Posicao -> Peca -> Carro -> Maybe Carro
movimentaC (b,d) (x,y) (Peca (Curva t) a ) c | t == Norte && ( dx >= dy)                    = Just c
                                             | t == Sul   && (dx + dy) <= 1                 = Just c
                                             | t == Este  && ( dx <= dy)                    = Just c
                                             | t == Oeste && (dx >= dy)                     = Just c
                                             | otherwise                                    = Nothing
    where dx = abs(b-toEnum x)
          dy = abs(d-toEnum y)

{-| Função que recebe um tempo e um carro e devolve um Maybe Carro.

Esta função vê o que acontece quando o carro choca contra uma parede.

>>> ricochete 2 (Carro {posicao = (2.1,1.5), direcao = 45, velocidade = (0.5,0.8)})
Just (Carro {posicao = (3.2,1.4), direcao = 45.0, velocidade = (0.5,0.8)})
-}
ricochete :: Tempo -> Carro -> Maybe Carro
ricochete t (Carro { posicao = x , direcao = a, velocidade = (0,0)})      = Just (Carro { posicao = x , direcao = a, velocidade = (0,0)})
ricochete t (Carro { posicao = (x,y), direcao = a, velocidade = (vx,vy)}) = Just (Carro { posicao = (x+(x-t*vx),y+(y-t*vy)), direcao = a, velocidade = (-vx,-vy)})

{-| Função que recebe um tempo e um carro e devolve um Maybe Carro.

Esta função vê o que acontece quando o carro choca contra uma parede, numa peça do tipo curva.

>>> richocheteCurva 2 (Carro {posicao = (2.1,1.5), direcao = 45, velocidade = (0.5,0.8)}) 
Just (Carro {posicao = (3.2,1.4), direcao = 45.0, velocidade = (0.5,0.8)})
-}
richocheteCurva :: Tempo -> Carro -> Maybe Carro
richocheteCurva t (Carro { posicao = (x,y) , direcao = a, velocidade = (vx,vy)}) = Just (Carro { posicao = (x+(x-t*vx),y+(y-t*vy)), direcao = a, velocidade = (-vx,-vy)})







a1 = ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
       [Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],
       [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],2,(Carro {posicao = (2.5,1.5), direcao = 45, velocidade = (1,0)}))

a2= ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
      [Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Lava 0],
      [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],2,(Carro {posicao = (2.5,1.5), direcao = 45, velocidade = (1,0)}))

a3 = ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
    [Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],
    [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],
    [Peca Lava 0,Peca (Curva Oeste) 0,Peca (Rampa Este) 0,Peca (Curva Sul) 1,Peca Lava 0],
    [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],2,(Carro {posicao = (2.5,1.5), direcao = 45, velocidade = (1,0)}))


a4 = ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
  [Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],
  [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],
  [Peca Lava 0,Peca (Curva Oeste) 0,Peca (Rampa Este) 0,Peca (Curva Sul) 1,Peca Lava 0],
  [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],2,(Carro {posicao = (3.5,2.1), direcao = 45, velocidade = (0,1)}))

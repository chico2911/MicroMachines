      {-|
Module      : Tarefa4_2017li1g2
Description : Módulo da Tarefa 4 para LI1 17/18

Módulo para a realização da Tarefa 4 de LI1 em 2017/18.
-}
module Tarefa4_2017li1g2 where

import LI11718
import Tarefa3_2017li1g2

{-|
O testes a serem considerados pelo sistema de /feedback/
para a função 'atualiza'.
-}
testesT4 :: [(Tempo,Jogo,Acao)]
testesT4 = [(1,Jogo m1 p1 [d1] [3] [[(3,1)]], Acao True False False False Nothing),
            (1,Jogo m1 p1 [d1] [3] [[(3,1)]], Acao True False False False (Just 0)),
            (1,Jogo m1 p1 [d1,d2,d3,d4] [3,1,5,2] [[(4,1)],[(3,2)],[(5,6)],[(4,7)]], Acao True False True False (Just 3)),
            (1,Jogo m1 p1 [d1,d2,d3,d4] [1,5,2,3] [[(4,1)],[(3,2)],[(5,6)],[(4,7)]], Acao True True False False (Just 3)),
            (1,Jogo m1 p1 [d1,d2,d3,d7] [1,5,2,3] [[(4,1)],[(3,2)],[(5,6)],[(4,7)]], Acao True True False False (Just 1)), 
            (1,Jogo m1 p1 [d1,d2,d3,d7] [1,5,2,3] [[(4,1)],[(3,2)],[(5,6)],[(4,7)]], Acao True True False False (Just 2)),  
            (1,Jogo m1 p1 [d1,d2,d3,d7] [1,5,2,3] [[(4,1)],[(3,2)],[(5,6)],[(4,7)]], Acao True True False False (Just 0)),
            (1,Jogo m1 p1 [d2] [3] [[(3,1)]], Acao False False False False Nothing),
            (1,Jogo m1 p1 [d2] [3] [[(3,1)]], Acao False False False False Nothing),
            (1,Jogo m1 p1 [d3] [3] [[(3,1)]], Acao False False False False Nothing),
            (1,Jogo m1 p1 [d3] [3] [[(3,1)]], Acao False False False False (Just 0)),
            (1,Jogo m1 p1 [d1] [3] [[(3,1)]], Acao True True True True Nothing)]

p1 = Propriedades 1 2 3 2 5 30

d1 = Carro (2.5,1.5) 0 (1,1)
d2 = Carro (3.5,2.5) 56 (1,1)
d5 = Carro (3.5,2.5) 45 (1,1)
d3 = Carro (3.5,3.5) 0 (1,1)
d4 = Carro (4.5,5.5) 35 (2,1)
d7 = Carro (4.5,5.5) 180 (2,1)
d6 = Carro (2.3,4.3) 37 (1,1)
d8 = Carro (2.3,4.3) 25 (1,1)
d9 = Carro (2.3,4.3) 100 (-3,-4)

m1 = Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                        [Peca Lava 0,Peca (Curva Norte) (-2),Peca (Rampa Este) (-2),Peca Recta (-1),Peca (Rampa Oeste) (-2),Peca (Curva Este) (-2),Peca Lava 0],
                        [Peca Lava 0,Peca (Rampa Sul) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-2),Peca Lava 0],
                        [Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0],
                        [Peca Lava 0,Peca (Rampa Norte) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) (-2),Peca Lava 0],
                        [Peca Lava 0,Peca (Curva Oeste) (-2),Peca (Rampa Este) (-2),Peca Recta (-1),Peca (Rampa Oeste) (-2),Peca (Curva Sul) (-2),Peca Lava 0],
                        [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]




{-|
Função usada para atualizar o estado do jogo dadas as
ações de um jogador num determinado período de tempo.
-}
atualiza :: Tempo -- ^ a duração da ação
         -> Jogo  -- ^ o estado do jogo
         -> Int   -- ^ o identificador do jogador
         -> Acao  -- ^ a ação tomada pelo jogador
         -> Jogo  -- ^ o estado atualizado do jogo

atualiza t e j a | j+1> length(carros e) = e
                 | otherwise = Jogo  (mapa e) (pista e) (atualizaCarros e t j a) (atualizaNitro a j t (nitros e)) (atualizaHistorico e j)

    
data Polar = Polar (Double,Double)  

{-/ Função que calcula a tangente do ângulo formado por (x,y).
-}
mytg :: Velocidade -> Double
mytg (x,y) = if x == 0 
             then if y > 0 
                  then (pi/2) 
                  else -(pi/2) 
              else if y == 0 
              then if x > 0  
                   then 0 
                   else pi 
              else atan (y/x)

{-| Função que recebe uma lista de carros e um int, identificando o carro pretendido.

>>> idCarro [Carro (2.5,1.5) 0 (1,1),Carro (3.5,2.5) 90 (1,1)] 1
Carro (3.5,2.5) 90 (1,1)
-} 
idCarro :: [Carro] -> Int -> Carro
idCarro c i | i>length c - 1 = error "Pedida posicão não existente"
            | otherwise = (!!) c i


{-/ Função que converte valores em graus para radianos.
>>> angRad 180
pi -}
angRad :: Double -> Double
angRad a = a * pi/180



{-/ Função que recebe uma lista de carros, procedendo à sua atualização.
-}
atualizaCarros :: Jogo -> Tempo -> Int -> Acao -> [Carro]
atualizaCarros j t i (Acao a1 a2 a3 a4 (Just s)) | s/=i && s<=length(carros j)+1 = insereCarro (insereCarro (carros j) (atualizaCarroN j t (idCarro (carros j) s) i) s) (atualizaCarro j t (idCarro (carros j) i) i (Acao a1 a2 a3 a4 (Just s))) i
                                                 | s==i = insereCarro (carros j) (atualizaCarro j t (idCarro (carros j) i) i (Acao a1 a2 a3 a4 (Just s))) i
                                                 | otherwise = error "Não Existe Carro para o nitro aplicado"

atualizaCarros j t i (Acao a1 a2 a3 a4 Nothing) = insereCarro (carros j) (atualizaCarro j t (idCarro (carros j) i) i (Acao a1 a2 a3 a4 Nothing)) i


{-/ Função que auxilia a função atualizaCarros. Atuliza o carro de outro jogador em que o utilizador utiliza o nitro.
-}
atualizaCarroN :: Jogo -> Tempo -> Carro -> Int -> Carro
atualizaCarroN j t (Carro x y (a,b)) i = Carro x y (a+(tempoNitro (nitros j) i t)*(fst(acNitro (pista j) y (a,b))),(b+(tempoNitro (nitros j) i t)*(snd(acNitro (pista j) y (a,b)))))

{-/ Função que auxilia a função atualizaCarros. Atuliza o carro do utilizador)
-}
atualizaCarro :: Jogo -> Tempo -> Carro -> Int -> Acao -> Carro 
atualizaCarro j t (Carro x y z) i a  = (Carro x (atualizaDirecao t j y a) (atualizaVelocidade i t x j y z a)) 

-----------------------------------------------------------------------------------------------------------
{-/ Função que auxilia a função atualizaCarro. Atuliza a direção do carro dependentemente da Ação tomada pelo jogar.
-}
atualizaDirecao :: Tempo -> Jogo -> Angulo -> Acao -> Angulo
atualizaDirecao t j a (Acao _ _ e d _) | e == True && d == False = (a+t*k_roda (pista j)) 
                                       | d == True && e == False = (a-t*k_roda (pista j))
                                       | e == True && d == True = a
                                       | otherwise = a

{-/ Função insere um carro numa certa lista na posição pedida.
-}
insereCarro :: [Carro] -> Carro -> Int -> [Carro]
insereCarro [] c i = [c]
insereCarro (h:t) c i | i == 0 = c : t
                      | otherwise = h : insereCarro t c (i-1)
  
-----------------------------------------------------------------------------------------------------------
{-/ Função que auxilia a função atualizaCarro. Atualiza a velocidade do carro.
-}
atualizaVelocidade :: Int -> Tempo -> Ponto -> Jogo -> Angulo -> Velocidade -> Acao -> Velocidade
atualizaVelocidade i t x j a v (Acao n f _ _ Nothing) | n == True && f == False  = soma x t a (mapa j) v (pista j) True
                                                      | f == True && n == False  = soma x t a (mapa j) v (pista j) False
                                                      | n == True && f == True   = (fst(v)+t*(fst(carteG (getPeca (mapa j) x) (pista j))+fst(vetorA a (-fst(v),-snd(v)) (pista j))+fst(pC(polarP a v (pista j)))),snd(v)+t*(snd(carteG (getPeca (mapa j) x)(pista j))+snd(vetorA a (-fst(v),-snd(v)) (pista j))+snd(pC(polarP a v (pista j)))))
                                                      | n == False && f == False = (fst(v)+t*(fst(carteG (getPeca (mapa j) x) (pista j))+fst(vetorA a (-fst(v),-snd(v)) (pista j))+fst(pC(polarP a v (pista j)))),snd(v)+t*(snd(carteG (getPeca (mapa j) x)(pista j))+snd(vetorA a (-fst(v),-snd(v)) (pista j))+snd(pC(polarP a v (pista j)))))

atualizaVelocidade i t x j a v (Acao n f d e (Just s))  | (temNitro (nitros j) i) && s == i && n == True && f == False  = (fst(soma x t a (mapa j) v (pista j) True) + (tempoNitro (nitros j) i t)*fst(acNitro (pista j) a v), snd(soma x t a (mapa j) v (pista j) True) + (tempoNitro (nitros j) i t)*snd(acNitro (pista j) a v))
                                                        | (temNitro (nitros j) i) && s == i && f == True && n == False = (fst(soma x t a (mapa j) v (pista j) False) + (tempoNitro (nitros j) i t)*fst(acNitro (pista j) a v), snd(soma x t a (mapa j) v (pista j) False) + (tempoNitro (nitros j) i t)*snd(acNitro (pista j) a v))
                                                        | (temNitro (nitros j) i) && s == i && n == True && f == True  = (fst(v)+(tempoNitro (nitros j) i t)*fst(acNitro (pista j) a v)+t*(fst(carteG (getPeca (mapa j) x) (pista j))+fst(vetorA a (-fst(v),-snd(v)) (pista j))+fst(pC(polarP a v (pista j)))),snd(v)+(tempoNitro (nitros j) i t)*snd(acNitro(pista j) a v)+t*(snd(carteG (getPeca (mapa j) x)(pista j))+snd(vetorA a (-fst(v),-snd(v)) (pista j))+snd(pC(polarP a v (pista j)))))
                                                        | (temNitro (nitros j) i) && s == i && n == False && f == False = (fst(v)+(tempoNitro (nitros j) i t)*fst(acNitro (pista j) a v)+t*(fst(carteG (getPeca (mapa j) x) (pista j))+fst(vetorA a (-fst(v),-snd(v)) (pista j))+fst(pC(polarP a v (pista j)))),snd(v)+(tempoNitro (nitros j) i t)*snd(acNitro (pista j) a v)+t*(snd(carteG (getPeca (mapa j) x)(pista j))+snd(vetorA a (-fst(v),-snd(v)) (pista j))+snd(pC(polarP a v (pista j)))))
                                                        | otherwise = atualizaVelocidade i t x j a v (Acao n f d e Nothing)
                                                        

temNitro :: [Tempo] -> Int -> Bool
temNitro t i | ((!!) t i) == 0 = False
             | otherwise = True

tempoNitro :: [Tempo] -> Int -> Tempo -> Double
tempoNitro x s t |  ((!!) x s) - t >= 0 = t
                 | otherwise = ((!!) x s)

{-/ Função que soma todos os vetores com a velocidade.
-}
soma :: Ponto -> Tempo -> Angulo -> Mapa -> Velocidade -> Propriedades -> Bool -> (Double,Double)
soma g t a m (x,y) p b | b == True = (x+t*(fst(carteac (a) p)+fst(vetorA a (-x,-y) p)+fst(carteG (getPeca m g) p)+fst(pC(polarP a (x,y) p))),y+t*(snd(vetorA a (-x,-y) p)+snd(carteG (getPeca m g) p)+snd(pC(polarP a (x,y) p))+snd(carteac a p)))
                       | otherwise = (x-t*(fst(carteac a p)+fst(vetorA a (-x,-y) p)+fst(carteG (getPeca m g) p)+fst(pC(polarP a (x,y) p))),y+t*(snd(vetorA a (-x,-y) p)+snd(carteG (getPeca m g) p)+snd(pC(polarP a (x,y) p))-snd(carteac a p)))
---------------------------------------------------------------------------------------------------------------------
{-/ Função que calcula o vetor atrito.
-}
vetorA :: Angulo -> (Double,Double) -> Propriedades -> (Double,Double)
vetorA a (x,y) p = (x*k_atrito p,y*k_atrito p)

{-/ Função que calcula o vetor resultante da resistência dos pneus.
-}
polarP :: Angulo -> (Double,Double) -> Propriedades -> Polar
polarP a (x,y) p = if abs((angRad a)-mytg (x,y))> pi then Polar ((k_pneus p) * (sin((angRad a)-mytg (x,y)))*normaV(x,y) , angRad a + pi/2)
                                                     else Polar ((k_pneus p) * (sin((angRad a)-mytg (x,y)))*normaV(x,y), angRad a - pi/2)
{-/ Função que calcula a norma da velocidade.
-}
normaV :: Velocidade -> Double
normaV (x,y) = sqrt(x^2+y^2)

{-/ Função que calcula o vetor do peso.
-}
carteG :: Tipo -> Propriedades -> Velocidade
carteG o p | o == Rampa Norte = (0,k_peso p)
           | o == Rampa Sul   = (0,-k_peso p)
           | o == Rampa Este  = (-k_peso p,0)
           | o == Rampa Oeste = (k_peso p,0)
           | otherwise = (0,0)
---------------------------------------------------------------------------------------------------------------------   
{-/ Função que calcula o vetor aceleração.
-}
carteac :: Double -> Propriedades -> (Double,Double)
carteac a p = (cos (angRad a) * k_acel p,-sin (angRad a) * k_acel p)

{-/ Função que transforma vetores ___Polares___ em ___Cartesianos___.
-}
pC :: Polar -> (Double,Double)
pC (Polar (x,y)) = (x*cos y,-x*sin y)

---------------------------------------------------------------------------------------------------------------------
{-/ Função que atravês de um Mapa e de um Ponto retorna o Tipo da peça nesse Ponto.
-}
getPeca :: Mapa -> Ponto -> Tipo
getPeca (Mapa (p,e) t) (x,y) = peca ((!!) ((!!) t (truncate y)) (truncate x)) 
    where peca :: Peca -> Tipo
          peca (Peca m a) = m

---------------------------------------------------------------------------------------------------------------------
{-/ Função que calcula a Posição em que o carro se encontra.
-}
posicaoCarro :: Carro -> Posicao
posicaoCarro (Carro (a,b) c d) = (truncate a,truncate b)
---------------------------------------------------------------------------------------------------------------------
atualizaNitro :: Acao -> Int -> Tempo -> [Tempo] -> [Tempo]
atualizaNitro (Acao _ _ _ _ Nothing) i t (x:xs) = (x:xs)
atualizaNitro (Acao a b e d (Just c)) i t (x:xs) | i == 0   = (subtraiNitro t x) : xs
                                                 | otherwise = x : atualizaNitro (Acao a b e d (Just c)) (i-1) t xs 
 

subtraiNitro :: Tempo -> Tempo -> Tempo
subtraiNitro t ti | ti == 0 = 0
                  | ti - t >= 0 = ti - t
                  | otherwise = 0
{-/ Função que calcula o vetor resultante da utilização do Nitro.
-}
acNitro:: Propriedades -> Angulo -> Velocidade -> Velocidade
acNitro p a (x,y) = (cos(angRad a)*(k_nitro p),-sin(angRad a)*(k_nitro p))

{-/ Função que atualiza o Histórico do Jogo.
-}
atualizaHistorico :: Jogo -> Int -> [[Posicao]]
atualizaHistorico j i = insereHistorico (historico j) (aux ((!!) (historico j) i) (idCarro (carros j) i ) i) i
        where aux :: [Posicao] -> Carro -> Int -> [Posicao]
              aux (x:xs) c i | head(x:xs) /= (posicaoCarro c) = (posicaoCarro c) : (x:xs)
                             | otherwise = (x:xs)
              aux [] c i = [(posicaoCarro c)]

{-/ Função que insere uma lista de Posições num Histórico na posição da lista desejada.
-}
insereHistorico :: [[Posicao]] -> [Posicao] -> Int -> [[Posicao]]
insereHistorico [[]] x i | i == 0 = [x]
                         | otherwise = [] : insereHistorico [[]] x (i-1)

insereHistorico (h:t) x i | i == 0 = x : t
                          | otherwise = h : insereHistorico t x (i-1)
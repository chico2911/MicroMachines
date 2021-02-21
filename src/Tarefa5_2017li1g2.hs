{-|
Module      : Tarefa5_2017li1g125
Description : Módulo da Tarefa 5 para LI1 17/18

Módulo para a realização da Tarefa 5 de LI1 em 2017/18.
-}
module Main where

import LI11718
import Graphics.Gloss                       
import Graphics.Gloss.Data.Picture          
import Graphics.Gloss.Interface.Pure.Game   
import Tarefa3_2017li1g2
import Tarefa4_2017li1g2
import GHC.Float
{-|
Função principal usada para animar um jogo completo.
Compilar com o GHC.
-}
-- | Função principal que invoca o jogo.
main :: IO ()
main = do
    inicio <- imgCarro
    joga inicio


j :: Jogo
j = Jogo {mapa = Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                    [Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0],
                                    [Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava (0),Peca Lava (0),Peca Recta (-1),Peca Lava 0],
                                    [Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava (0),Peca Lava (0),Peca Recta (-1),Peca Lava 0],
                                    [Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava (0),Peca Lava (0),Peca Recta (-1),Peca Lava 0],
                                    [Peca Lava 0,Peca (Curva Oeste) (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0],
                                    [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]], 
                                    pista = Propriedades {k_atrito = 1.0, k_pneus = 2.0, k_acel = 3.0, k_peso = 2.0, k_nitro = 5.0, k_roda = 30.0}, 
                                    carros = [Carro {posicao = (2.5,1.5), direcao = 0.0, velocidade = (1,0)}], nitros = [3.0], historico = [[]]}--Adicionar Jogo ao Estado

type Estado = (Int,Int,(Float,Float),(Float,Float),Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Temp,Acao,Jogo)

-- | 
type Teclas = (Bool,Bool,Bool,Bool)
type Temp = Float

imgCarro :: IO Estado 
imgCarro = do carro <- loadBMP "Pictures/carro.bmp"
              curvaEste <- loadBMP "Pictures/curvaEste.bmp"
              curvaOeste <- loadBMP "Pictures/curvaOeste.bmp"
              curvaNorte <- loadBMP "Pictures/curvaNorte.bmp"
              curvaSul <- loadBMP "Pictures/curvaSul.bmp"
              retaRampa <- loadBMP "Pictures/retaRampa.bmp"
              lava <- loadBMP "Pictures/lava.bmp"
              menu1 <- loadBMP "Pictures/mmws_banner_1_.bmp"
              menu2 <- loadBMP "Pictures/mmws_banner_2_.bmp"
              return (1,0,(135*fromIntegral(fst(alturaLargura j)),135*fromIntegral(snd(alturaLargura j))),((posicaoCarroX j 0),(posicaoCarroY j 0)),carro,curvaEste,curvaOeste,curvaNorte,curvaSul,retaRampa,lava,menu1,menu2,0,(Acao False False False False Nothing),j)


posicaoCarroFLoat :: Carro -> (Float,Float)
posicaoCarroFLoat (Carro (x,y) a s) = (double2Float x, double2Float y)

atualizaCarrosJogo :: Jogo -> Jogo
atualizaCarrosJogo (Jogo t p c n h) = Jogo t p (posicaonomapa c) n h

posicaonomapa :: [Carro] -> [Carro]
posicaonomapa [] = []
posicaonomapa ((Carro (x,y) a v):xs) = Carro (135*fromIntegral(fst(alturaLargura j))-135*x,135*fromIntegral(snd(alturaLargura j))-135*y) a v : posicaonomapa xs




moveCarroP :: Maybe Carro -> Estado -> (Float,Float)
moveCarroP (Just (Carro (x,y) r d )) (k,i,(xMapa,yMapa),(xCarro,yCarro),carro,curvaEste,curvaOeste,curvaNorte,curvaSul,retaRampa,lava,menu1,menu2,time,t,(Jogo m c n h a)) = (135*(double2Float x),135*fromIntegral(fst(alturaLargura j))-135*(double2Float y))
moveCarroP (Nothing) (k,i,(xMapa,yMapa),(xCarro,yCarro),carro,curvaEste,curvaOeste,curvaNorte,curvaSul,retaRampa,lava,m1,m2,time,t,(Jogo m c n h a)) = (0,0)


desenhaEstado :: Estado -> Picture
desenhaEstado (u,i,(xMapa,yMapa),(x,y),carro,curvaEste,curvaOeste,curvaNorte,curvaSul,retaRampa,lava,menu1,menu2,time,t,j) | u == 1 = Translate 0 0 menu1
                                                                                                                           | u == 2 = Translate 0 0 menu2
                                                                                                                           | otherwise = Pictures [borda,tabuleiro,figura]
    where
    -- borda do mapa a preto, centrada na janela
    borda = Translate (-(xMapa+20)/2) (-(yMapa+20)/2) $ Color black (Polygon [(0,0),(0,yMapa + 20),(xMapa + 20,yMapa + 20),(xMapa + 20,0)])
    -- mapa a branco, centrado na janela
    tabuleiro = Translate (-xMapa/2) (-yMapa/2) $ background (picTab (u,i,(xMapa,yMapa),(x,y),carro,curvaEste,curvaOeste,curvaNorte,curvaSul,retaRampa,lava,menu1,menu2,time,t,j) (tabPos (mapa j))) (67.5,125*fromIntegral(snd(alturaLargura j)))
    -- carro dentro do mapa do jogo
    figura = Translate (-xMapa/2) (-yMapa/2) $ Translate (posicaoCarroX j i) (posicaoCarroY j i) $ rotate (double2Float (encontraDir ((!!) (carros j) i))) $ scale 0.75 0.75 carro 

reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (SpecialKey KeyDown)    Down _ _) (1,i,x,y,z,e,oe,n,s,rr,l,m1,m2,t,a,j) = (2,i,x,y,z,e,oe,n,s,rr,l,m1,m2,t,a,j)
reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) (2,i,x,y,z,e,oe,n,s,rr,l,m1,m2,t,a,j) = (1,i,x,y,z,e,oe,n,s,rr,l,m1,m2,t,a,j)
reageEvento (EventKey (SpecialKey KeyEnter)    Down _ _) (1,i,x,y,z,e,oe,n,s,rr,l,m1,m2,t,a,j) = (3,i,x,y,z,e,oe,n,s,rr,l,m1,m2,t,a,j) 
reageEvento (EventKey (SpecialKey KeyEnter)    Down _ _) (2,i,x,y,z,e,oe,n,s,rr,l,m1,m2,t,a,j) = (4,i,x,y,z,e,oe,n,s,rr,l,m1,m2,t,a,j) 

reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) (3,i,x,y,z,e,oe,n,s,rr,l,m1,m2,t,(Acao t1 t2 t3 t4 t5),(Jogo (Mapa o ta) p c d h)) =(3,i,x,y,z,e,oe,n,s,rr,l,m1,m2,t,(Acao True t2 t3 t4 t5),(Jogo (Mapa o ta) p c d h))
reageEvento (EventKey (SpecialKey KeyDown)  Down _ _) (3,i,x,y,z,e,oe,n,s,rr,l,m1,m2,t,(Acao t1 t2 t3 t4 t5),(Jogo (Mapa o ta) p c d h)) = (3,i,x,y,z,e,oe,n,s,rr,l,m1,m2,t,(Acao t1 True t3 t4 t5),(Jogo (Mapa o ta) p c d h)) 
reageEvento (EventKey (SpecialKey KeyLeft)  Down _ _) (3,i,x,y,z,e,oe,n,s,rr,l,m1,m2,t,(Acao t1 t2 t3 t4 t5),(Jogo (Mapa o ta)  p c d h)) =(3,i,x,y,z,e,oe,n,s,rr,l,m1,m2,t,(Acao t1 t2 True t4 t5),(Jogo (Mapa o ta) p c d h)) 
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (3,i,x,y,z,e,oe,n,s,rr,l,m1,m2,t,(Acao t1 t2 t3 t4 t5),(Jogo (Mapa o ta) p c d h)) = (3,i,x,y,z,e,oe,n,s,rr,l,m1,m2,t,(Acao t1 t2 t3 True t5),(Jogo (Mapa o ta) p c d h)) 
reageEvento (EventKey (SpecialKey KeyUp)    Up _ _) (3,i,x,y,z,e,oe,n,s,rr,l,m1,m2,t,(Acao t1 t2 t3 t4 t5),(Jogo (Mapa o ta) p c d h))  =  (3,i,x,y,z,e,oe,n,s,rr,l,m1,m2,t,(Acao False t2 t3 t4 t5),(Jogo (Mapa o ta) p c d h))
reageEvento (EventKey (SpecialKey KeyDown)  Up _ _) (3,i,x,y,z,e,oe,n,s,rr,l,m1,m2,t,(Acao t1 t2 t3 t4 t5),(Jogo (Mapa o ta) p c d h))  =  (3,i,x,y,z,e,oe,n,s,rr,l,m1,m2,t,(Acao t1 False t3 t4 t5),(Jogo (Mapa o ta) p c d h))
reageEvento (EventKey (SpecialKey KeyLeft)  Up _ _) (3,i,x,y,z,e,oe,n,s,rr,l,m1,m2,t,(Acao t1 t2 t3 t4 t5),(Jogo (Mapa o ta) p c d h))  =  (3,i,x,y,z,e,oe,n,s,rr,l,m1,m2,t,(Acao t1 t2 False t4 t5),(Jogo (Mapa o ta) p c d h))
reageEvento (EventKey (SpecialKey KeyRight) Up _ _) (3,i,x,y,z,e,oe,n,s,rr,l,m1,m2,t,(Acao t1 t2 t3 t4 t5),(Jogo (Mapa o ta) p c d h))  =  (3,i,x,y,z,e,oe,n,s,rr,l,m1,m2,t,(Acao t1 t2 t3 False t5),(Jogo (Mapa o ta) p c d h))
reageEvento _ mapa = mapa 

auxi c l t i e = insereCarro c (atualizaPosCarro (mmovimenta l (float2Double t) ((!!) c i)) e) i

reageTempo :: Float -> Estado -> Estado
reageTempo t' (1,i,x,y,z,e,oe,n,s,rr,l,m1,m2,t,a,j) = (1,i,x,y,z,e,oe,n,s,rr,l,m1,m2,t,a,j)
reageTempo t' (2,i,x,y,z,e,oe,n,s,rr,l,m1,m2,t,a,j) = (2,i,x,y,z,e,oe,n,s,rr,l,m1,m2,t,a,j)


reageTempo t' (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True True True True t5),(Jogo (Mapa o ta) p c d h))  = (3,i,x,(moveCarroP (mmovimenta ta (float2Double(t')) ((!!) c i)) (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True True True True t5),(Jogo (Mapa o ta) p c d h))),z,e,oe,n,s,rr,l,m1,m2,t+t',(Acao True True True True t5),atualiza (float2Double(t')) (Jogo (Mapa o ta) p (auxi c ta t' i (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True True True True t5),(Jogo (Mapa o ta) p c d h))) d h) i (Acao True True True True t5))
reageTempo t' (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True False False False t5),(Jogo (Mapa o ta) p c d h))  = (3,i,x,(moveCarroP (mmovimenta ta (float2Double(t')) ((!!) c i)) (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True False False False t5),(Jogo (Mapa o ta) p c d h))),z,e,oe,n,s,rr,l,m1,m2,t+t',(Acao True False False False t5),atualiza (float2Double(t')) (Jogo (Mapa o ta) p (auxi c ta t' i (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True False False False t5),(Jogo (Mapa o ta) p c d h))) d h) i (Acao True False False False t5))
reageTempo t' (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True True False False t5),(Jogo (Mapa o ta) p c d h))  = (3,i,x,(moveCarroP (mmovimenta ta (float2Double(t')) ((!!) c i)) (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True True False False t5),(Jogo (Mapa o ta) p c d h))),z,e,oe,n,s,rr,l,m1,m2,t+t',(Acao True True False False t5),atualiza (float2Double(t')) (Jogo (Mapa o ta) p (auxi c ta t' i (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True False False False t5),(Jogo (Mapa o ta) p c d h))) d h) i (Acao True True False False t5))
reageTempo t' (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True False True False t5),(Jogo (Mapa o ta) p c d h))  = (3,i,x,(moveCarroP (mmovimenta ta (float2Double(t')) ((!!) c i)) (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True False True False t5),(Jogo (Mapa o ta) p c d h))),z,e,oe,n,s,rr,l,m1,m2,t+t',(Acao True False True False t5),atualiza (float2Double(t')) (Jogo (Mapa o ta) p (auxi c ta t' i (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True False False False t5),(Jogo (Mapa o ta) p c d h))) d h) i (Acao True False True False t5))
reageTempo t' (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True False False True t5),(Jogo (Mapa o ta) p c d h))  = (3,i,x,(moveCarroP (mmovimenta ta (float2Double(t')) ((!!) c i)) (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True False False True t5),(Jogo (Mapa o ta) p c d h))),z,e,oe,n,s,rr,l,m1,m2,t+t',(Acao True False False True t5),atualiza (float2Double(t')) (Jogo (Mapa o ta) p (auxi c ta t' i (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True False False False t5),(Jogo (Mapa o ta) p c d h))) d h) i (Acao True False False True t5))
reageTempo t' (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True True False True t5),(Jogo (Mapa o ta) p c d h))  = (3,i,x,(moveCarroP (mmovimenta ta (float2Double(t')) ((!!) c i)) (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True True False True t5),(Jogo (Mapa o ta) p c d h))),z,e,oe,n,s,rr,l,m1,m2,t+t',(Acao True True False True t5),atualiza (float2Double(t')) (Jogo (Mapa o ta) p (auxi c ta t' i (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True False False False t5),(Jogo (Mapa o ta) p c d h))) d h) i (Acao True True False True t5))
reageTempo t' (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True True True False t5),(Jogo (Mapa o ta) p c d h))  = (3,i,x,(moveCarroP (mmovimenta ta (float2Double(t')) ((!!) c i)) (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True True True False t5),(Jogo (Mapa o ta) p c d h))),z,e,oe,n,s,rr,l,m1,m2,t+t',(Acao True True True False t5),atualiza (float2Double(t')) (Jogo (Mapa o ta) p (auxi c ta t' i (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True False False False t5),(Jogo (Mapa o ta) p c d h))) d h) i (Acao True True True False t5))
reageTempo t' (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao False True True False t5),(Jogo (Mapa o ta) p c d h))  = (3,i,x,(moveCarroP (mmovimenta ta (float2Double(t')) ((!!) c i)) (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao False True True False t5),(Jogo (Mapa o ta) p c d h))),z,e,oe,n,s,rr,l,m1,m2,t+t',(Acao False True True False t5),atualiza (float2Double(t')) (Jogo (Mapa o ta) p (auxi c ta t' i (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True False False False t5),(Jogo (Mapa o ta) p c d h))) d h) i (Acao False True True False t5))
reageTempo t' (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao False True False True t5),(Jogo (Mapa o ta) p c d h))  = (3,i,x,(moveCarroP (mmovimenta ta (float2Double(t')) ((!!) c i)) (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao False True False True t5),(Jogo (Mapa o ta) p c d h))),z,e,oe,n,s,rr,l,m1,m2,t+t',(Acao False True False True t5),atualiza (float2Double(t')) (Jogo (Mapa o ta) p (auxi c ta t' i (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True False False False t5),(Jogo (Mapa o ta) p c d h))) d h) i (Acao False True False True t5))
reageTempo t' (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao False True True True t5),(Jogo (Mapa o ta) p c d h))  = (3,i,x,(moveCarroP (mmovimenta ta (float2Double(t')) ((!!) c i)) (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao False True True True t5),(Jogo (Mapa o ta) p c d h))),z,e,oe,n,s,rr,l,m1,m2,t+t',(Acao False True True True t5),atualiza (float2Double(t')) (Jogo (Mapa o ta) p (auxi c ta t' i (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True False False False t5),(Jogo (Mapa o ta) p c d h))) d h) i (Acao False True True True t5))
reageTempo t' (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao False False True True t5),(Jogo (Mapa o ta) p c d h))  = (3,i,x,(moveCarroP (mmovimenta ta (float2Double(t')) ((!!) c i)) (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao False False True True t5),(Jogo (Mapa o ta) p c d h))),z,e,oe,n,s,rr,l,m1,m2,t+t',(Acao False False True True t5),atualiza (float2Double(t')) (Jogo (Mapa o ta) p (auxi c ta t' i (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True False False False t5),(Jogo (Mapa o ta) p c d h))) d h) i (Acao False False True True t5))
reageTempo t' (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao False True False False t5),(Jogo (Mapa o ta) p c d h))  = (3,i,x,(moveCarroP (mmovimenta ta (float2Double(t')) ((!!) c i)) (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao False True False False t5),(Jogo (Mapa o ta) p c d h))),z,e,oe,n,s,rr,l,m1,m2,t+t',(Acao False True False False t5),atualiza (float2Double(t')) (Jogo (Mapa o ta) p (auxi c ta t' i (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True False False False t5),(Jogo (Mapa o ta) p c d h))) d h) i (Acao False True False False t5))
reageTempo t' (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao False False True False t5),(Jogo (Mapa o ta) p c d h))  = (3,i,x,(moveCarroP (mmovimenta ta (float2Double(t')) ((!!) c i)) (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao False False True False t5),(Jogo (Mapa o ta) p c d h))),z,e,oe,n,s,rr,l,m1,m2,t+t',(Acao False False True False t5),atualiza (float2Double(t')) (Jogo (Mapa o ta) p (auxi c ta t' i (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True False False False t5),(Jogo (Mapa o ta) p c d h))) d h) i (Acao False False True False t5))
reageTempo t' (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao False False False True t5),(Jogo (Mapa o ta) p c d h))  = (3,i,x,(moveCarroP (mmovimenta ta (float2Double(t')) ((!!) c i)) (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao False False False True t5),(Jogo (Mapa o ta) p c d h))),z,e,oe,n,s,rr,l,m1,m2,t+t',(Acao False False False True t5),atualiza (float2Double(t')) (Jogo (Mapa o ta) p (auxi c ta t' i (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True False False False t5),(Jogo (Mapa o ta) p c d h))) d h) i (Acao False False False True t5))
reageTempo t' (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True False True True t5),(Jogo (Mapa o ta) p c d h))  = (3,i,x,(moveCarroP (mmovimenta ta (float2Double(t')) ((!!) c i)) (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True False True True t5),(Jogo (Mapa o ta) p c d h))),z,e,oe,n,s,rr,l,m1,m2,t+t',(Acao True False True True t5),atualiza (float2Double(t')) (Jogo (Mapa o ta) p (auxi c ta t' i (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True False False False t5),(Jogo (Mapa o ta) p c d h))) d h) i (Acao True False True True t5))
reageTempo t' (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao False False False False t5),(Jogo (Mapa o ta) p c d h)) = (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t+t',(Acao False False False False t5),atualiza (float2Double(t')) (Jogo (Mapa o ta) p (auxi c ta t' i (3,i,x,(px,py),z,e,oe,n,s,rr,l,m1,m2,t,(Acao True False False False t5),(Jogo (Mapa o ta) p c d h))) d h) i (Acao False False False False t5))

mmovimenta :: Tabuleiro -> Tempo -> Carro -> Maybe Carro
mmovimenta ta t (Carro x y (z,r)) = movimenta ta t (Carro x y (z,-r))



atualizaPosCarro :: Maybe Carro -> Estado -> Carro 
atualizaPosCarro (Just (Carro x y z)) (3,i,xi,(px,py),g,e,oe,n,s,rr,l,m1,m2,t,_,(Jogo (Mapa o ta) p c d h)) = (Carro x y z)
atualizaPosCarro Nothing (k,i,_,_,_,_,_,_,_,_,_,_,_,m1,m2,j) = (Carro (fromIntegral(fst(last((!!) (historico j) i))),(fromIntegral(snd(last ((!!) (historico j) i))))) 0 (0,0))

posicaoCarroX :: Jogo -> Int -> Float
posicaoCarroX (Jogo (Mapa o ta) p c n h) i = double2Float (posicaoAux ((!!) c i)(Jogo (Mapa o ta) p c n h))
    where posicaoAux (Carro x y z) (Jogo (Mapa o ta) p c n h) = 135*(fst(x))

posicaoCarroY :: Jogo -> Int -> Float
posicaoCarroY (Jogo (Mapa o ta) p c n h) i = double2Float (posicaoAux ((!!) c i) (Jogo (Mapa o ta) p c n h))    
    where posicaoAux (Carro x y z) (Jogo (Mapa o ta) p c n h) = 135*fromIntegral(fst(alturaLargura (Jogo (Mapa o ta) p c n h)))-135*(snd(x))


encontraDir :: Carro -> Angulo
encontraDir (Carro x y z) = y

-----------------------------------------------------------------------------------------------------------------------------------------------
tabPos ::  Mapa -> [[(Peca,Posicao)]]
tabPos (Mapa o t) = tabPosAux t (0,0)

tabPosAux :: Tabuleiro -> (Int, Int) -> [[(Peca,Posicao)]]
tabPosAux [] (a,b) = [[]]
tabPosAux (x:xs) (a,b) = pecasy x (a,b) : tabPosAux xs (a,b+1)


pecasy :: [Peca] -> (Int,Int) -> [(Peca,Posicao)]
pecasy [] (a,b) = []
pecasy (x:xs) (a,b) = pecasx x (a,b) : pecasy xs (a+1,b) 

pecasx :: Peca -> (Int,Int) -> (Peca,Posicao)
pecasx y x = (y,x)
------------------------------------------------------------------------------------------------------------------------------------------------

picTab :: Estado -> [[(Peca,Posicao)]] -> [[(Picture)]]
picTab e [] = []
picTab e (x:xs) = picTabA e x : picTab e xs

picTabA :: Estado -> [(Peca,Posicao)] -> [(Picture)]
picTabA e [] = []
picTabA (k,i,(xMapa,yMapa),(x,y),carro,curvaEste,curvaOeste,curvaNorte,curvaSul,retaRampa,lava,m1,m2,time,t,j) ((Peca w a,h):xs) | w == Lava = (lava) : picTabA (k,i,(xMapa,yMapa),(x,y),carro,curvaEste,curvaOeste,curvaNorte,curvaSul,retaRampa,lava,m1,m2,time,t,j) xs
                                                                                                                         | w == (Curva Oeste) = (curvaOeste) : picTabA (k,i,(xMapa,yMapa),(x,y),carro,curvaEste,curvaOeste,curvaNorte,curvaSul,retaRampa,lava,m1,m2,time,t,j) xs
                                                                                                                         | w == (Curva Este) = (curvaEste) : picTabA (k,i,(xMapa,yMapa),(x,y),carro,curvaEste,curvaOeste,curvaNorte,curvaSul,retaRampa,lava,m1,m2,time,t,j) xs
                                                                                                                         | w == (Curva Norte) = (curvaNorte) : picTabA (k,i,(xMapa,yMapa),(x,y),carro,curvaEste,curvaOeste,curvaNorte,curvaSul,retaRampa,lava,m1,m2,time,t,j) xs
                                                                                                                         | w == (Curva Sul) = (curvaSul) : picTabA (k,i,(xMapa,yMapa),(x,y),carro,curvaEste,curvaOeste,curvaNorte,curvaSul,retaRampa,lava,m1,m2,time,t,j) xs
                                                                                                                         | w == (Rampa Oeste) || w == (Rampa Este) || w == (Rampa Norte) || w == (Rampa Sul)  = (retaRampa) : picTabA (k,i,(xMapa,yMapa),(x,y),carro,curvaEste,curvaOeste,curvaNorte,curvaSul,retaRampa,lava,m1,m2,time,t,j) xs
                                                                                                                         | w == Recta = (retaRampa) : picTabA (k,i,(xMapa,yMapa),(x,y),carro,curvaEste,curvaOeste,curvaNorte,curvaSul,retaRampa,lava,m1,m2,time,t,j) xs


background :: [[Picture]] -> (Float,Float) -> Picture
background [] y = Pictures []
background (x:xs) (a,b) = Pictures (aux x (a,b) : [background xs (a,b-135)])
    where aux :: [(Picture)] -> (Float,Float) -> Picture
          aux [] x = Pictures []
          aux (x:xs) (a,b) = Pictures (backgroundAux x (a,b) : [aux xs (a+135,b)])

backgroundAux :: Picture -> (Float,Float) -> Picture
backgroundAux x y = Translate (fst(y)) (snd (y)) $ scale 1  1 x

alturaLargura :: Jogo -> (Int,Int)
alturaLargura (Jogo (Mapa o (x:xs)) p c n h) = (length (x:xs),length x)

joga :: Estado -> IO ()
joga inicio = play
    FullScreen 
    (greyN 0.5)                               
    60                                       
    inicio                                    
    desenhaEstado                             
    reageEvento                               
    reageTempo                                


    
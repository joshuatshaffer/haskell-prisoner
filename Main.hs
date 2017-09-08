module Main where

data Move = Cooperate | Defect deriving (Show, Eq)

type Player = [(Move,Move)] -> Double

alwaysC :: Player
alwaysC _ = 1
alwaysD :: Player
alwaysD _ = 0

randomPlayer :: Player
randomPlayer _ = 0.5

grudger :: Player
grudger h = if any ((== Defect) . snd) h then 0 else 1

titForTat :: Player
titForTat [] = 1
titForTat ((_,Cooperate):_) = 1
titForTat ((_,Defect):_) = 0

flipAsoc :: [(a, b)] -> [(b, a)]
flipAsoc = map (\(a,b)->(b,a))

data Tree4 = Node Double Tree4 Tree4 Tree4 Tree4 | Impos | Nil deriving (Show)

drawTree4 :: Tree4 -> String
drawTree4 = drawTree4' 0
  where
    drawTree4' i (Node p cc cd dc dd) = show p ++ " -> "

unfoldRound :: Player -> Player -> Int -> [(Move,Move)] -> Double -> Tree4
unfoldRound _  _  _ _ 0 = Impos
unfoldRound _  _  0 _ _ = Nil
unfoldRound p1 p2 n h p = Node p
                           (unfoldRound'' (Cooperate,Cooperate)   $ m1*m2)
                           (unfoldRound'' (Cooperate,Defect)  $ m1*(1-m2))
                           (unfoldRound'' (Defect,Cooperate)  $ (1-m1)*m2)
                           (unfoldRound'' (Defect,Defect) $ (1-m1)*(1-m2))
  where
    m1 = p1 h
    m2 = p2 $ flipAsoc h
    unfoldRound'' :: (Move,Move) -> Double -> Tree4
    unfoldRound'' m = unfoldRound p1 p2 (n-1) (m:h)

foldScore :: Double -> Double -> Tree4 -> (Double, Double)
foldScore s1 s2 Nil = (s1,s2)
foldScore _  _  Impos = (0,0)
foldScore s1 s2 (Node p cc cd dc dd) = (p * (a+b+c+d), p * (x+y+z+w))
  where
    (a,x) = foldScore (s1 + 3) (s2 + 3) cc
    (b,y) = foldScore (s1 + 0) (s2 + 5) cd
    (c,z) = foldScore (s1 + 5) (s2 + 0) dc
    (d,w) = foldScore (s1 + 1) (s2 + 1) dd

play :: Int -> Player -> Player -> (Double, Double)
play n' p1 p2 = rez n' [] 1 0 0
  where
    rez :: Int -> [(Move,Move)] -> Double -> Double -> Double -> (Double,Double)
    rez _ _ 0 _ _ = (0, 0)
    rez 0 _ p p1s p2s = (p * p1s, p * p2s)
    rez n h p p1s p2s = (p * (a+b+c+d), p * (x+y+z+w))
      where
        m1 = p1 h
        m2 = p2 $ flipAsoc h
        (a,x) = rez (n-1) ((Cooperate,Cooperate):h)   (m1*m2) (p1s + 3) (p2s + 3)
        (b,y) = rez (n-1) ((Cooperate,Defect):h)  (m1*(1-m2)) (p1s + 0) (p2s + 5)
        (c,z) = rez (n-1) ((Defect,Cooperate):h)  ((1-m1)*m2) (p1s + 5) (p2s + 0)
        (d,w) = rez (n-1) ((Defect,Defect):h) ((1-m1)*(1-m2)) (p1s + 1) (p2s + 1)

main :: IO ()
main = do
  print . foldScore 0 0 $ unfoldRound alwaysC alwaysC 13 [] 1
  print $ play 13 alwaysC alwaysC

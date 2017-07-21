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
main = print $ play 50 randomPlayer alwaysC

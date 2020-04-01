import Control.Exception
data Hanoi = Rods [Int] [Int] [Int]
  deriving (Show, Eq)

data Move = AB | AC | BA | BC | CA | CB
  deriving (Show, Eq)

a2, a3, a4 :: Hanoi
a2 = Rods [1,2] [] []
a3 = Rods [1,2,3] [] []
a4 = Rods [1,2,3,4] [] []


rodMove [] t = error "moving from empty rod"
rodMove (f:ft) [] = (ft, [f])
rodMove (f:ft) ts@(t:tt) = assert (f < t) (ft, f:ts)

move AB (Rods a b c) = let (na, nb) = rodMove a b in Rods na nb c
move AC (Rods a b c) = let (na, nc) = rodMove a c in Rods na b nc
move BA (Rods a b c) = let (nb, na) = rodMove b a in Rods na nb c
move BC (Rods a b c) = let (nb, nc) = rodMove b c in Rods a nb nc
move CA (Rods a b c) = let (nc, na) = rodMove c a in Rods na b nc
move CB (Rods a b c) = let (nc, nb) = rodMove c b in Rods a nb nc


sm moves AC (Rods [] b c) = (Rods [] b c, moves)
sm moves AB (Rods [] b c) = (Rods [] b c, moves)
sm moves BC (Rods a [] c) = (Rods a [] c, moves)
sm moves BA (Rods a [] c) = (Rods a [] c, moves)
sm moves CA (Rods a b []) = (Rods a b [], moves)
sm moves CB (Rods a b []) = (Rods a b [], moves)

sm moves AC (Rods (ha:ta) b c) = let (Rods _ bb bc, bmoves) = sm moves AB (Rods ta [] []) in sm (AC:bmoves) BC (Rods [] (bb++b) (bc++ha:c))

sm moves AB (Rods (ha:ta) b c) = let (Rods _ bb bc, bmoves) = sm moves AC (Rods ta [] []) in sm (AB:bmoves) CB (Rods [] (bb++ha:b) (bc++c))

sm moves BC (Rods a (hb:tb) c) = let (Rods ba _ bc, bmoves) = sm moves BA (Rods [] tb []) in sm (BC:bmoves) AC (Rods (ba++a) [] (bc++hb:c))

sm moves BA (Rods a (hb:tb) c) = let (Rods ba _ bc, bmoves) = sm moves BC (Rods [] tb []) in sm (BA:bmoves) CA (Rods (ba++hb:a) [] (bc++c))

sm moves CA (Rods a b (hc:tc)) = let (Rods ba bb _, bmoves) = sm moves CB (Rods [] [] tc) in sm (CA:bmoves) BA (Rods (ba++hc:a) (bb++b) [])

sm moves CB (Rods a b (hc:tc)) = let (Rods ba bb _, bmoves) = sm moves CA (Rods [] [] tc) in sm (CB:bmoves) AB (Rods (ba++a) (bb++hc:b) [])
 
getMoves = take 30 . snd . sm [] AC
run = scanr move a4 . getMoves $ a4

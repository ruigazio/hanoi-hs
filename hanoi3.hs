import Control.Exception

-- discs in rods A B C
data Hanoi = Rods [Int] [Int] [Int]
  deriving (Show, Eq)

-- move from A to B, A to C, ...
data Move = AB | AC | BA | BC | CA | CB
  deriving (Show, Eq)

getInitialHanoi n = Rods [1..n] [] []

-- check for invalid moves
rodMove [] t = error "moving from empty rod"
rodMove (f:ft) [] = (ft, [f])
rodMove (f:ft) ts@(t:tt) = assert (f < t) (ft, f:ts)

-- make a move
move AB (Rods a b c) = let (na, nb) = rodMove a b in Rods na nb c
move AC (Rods a b c) = let (na, nc) = rodMove a c in Rods na b nc
move BA (Rods a b c) = let (nb, na) = rodMove b a in Rods na nb c
move BC (Rods a b c) = let (nb, nc) = rodMove b c in Rods a nb nc
move CA (Rods a b c) = let (nc, na) = rodMove c a in Rods na b nc
move CB (Rods a b c) = let (nc, nb) = rodMove c b in Rods a nb nc

-- move whole stack of discs
stack AC (Rods [a] _ _) = [AC]
stack AB (Rods [a] _ _) = [AB]
stack BC (Rods _ [b] _) = [BC]
stack BA (Rods _ [b] _) = [BA]
stack CA (Rods _ _ [c]) = [CA]
stack CB (Rods _ _ [c]) = [CB]

stack AC (Rods (ha:ta) b c) = stack AB (Rods ta [] [])
  ++ AC : stack BC (Rods [] (ta++b) (ha:c))
stack AB (Rods (ha:ta) b c) = stack AC (Rods ta [] [])
  ++ AB : stack CB (Rods [] (ha:b) (ta++c))
stack BC (Rods a (hb:tb) c) = stack BA (Rods [] tb [])
  ++ BC : stack AC (Rods (tb++a) [] (hb:c))
stack BA (Rods a (hb:tb) c) = stack BC (Rods [] tb [])
  ++ BA : stack CA (Rods (hb:a) [] (tb++c))
stack CA (Rods a b (hc:tc)) = stack CB (Rods [] [] tc)
  ++ CA : stack BA (Rods (hc:a) (tc++b) [])
stack CB (Rods a b (hc:tc)) = stack CA (Rods [] [] tc)
  ++ CB : stack AB (Rods (tc++a) (hc:b) [])
 
getMoves :: Hanoi -> [Move]
getMoves = stack AC

makeMoves :: Hanoi -> [Move] -> [Hanoi]
makeMoves hanoi = scanl (flip move) hanoi

split :: (a -> b) -> (a -> c) -> a -> (b, c)
split f g x = (f x, g x)

apply :: (a -> b, a) -> b
apply = uncurry ($)

play = apply . split makeMoves getMoves
playWith = play . getInitialHanoi

main = print $ playWith 4

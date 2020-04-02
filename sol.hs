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
stack AC moves (Rods [a] _ _) = AC:moves
stack AB moves (Rods [a] _ _) = AB:moves
stack BC moves (Rods _ [b] _) = BC:moves
stack BA moves (Rods _ [b] _) = BA:moves
stack CA moves (Rods _ _ [c]) = CA:moves
stack CB moves (Rods _ _ [c]) = CB:moves

stack AC moves (Rods (ha:ta) b c) =
  let pmoves = stack AB moves (Rods ta [] [])
  in stack BC (AC:pmoves) (Rods [] (ta++b) (ha:c))

stack AB moves (Rods (ha:ta) b c) =
  let pmoves = stack AC moves (Rods ta [] [])
  in stack CB (AB:pmoves) (Rods [] (ha:b) (ta++c))

stack BC moves (Rods a (hb:tb) c) =
  let pmoves = stack BA moves (Rods [] tb [])
  in stack AC (BC:pmoves) (Rods (tb++a) [] (hb:c))

stack BA moves (Rods a (hb:tb) c) =
  let pmoves = stack BC moves (Rods [] tb [])
  in stack CA (BA:pmoves) (Rods (hb:a) [] (tb++c))

stack CA moves (Rods a b (hc:tc)) =
  let pmoves = stack CB moves (Rods [] [] tc)
  in stack BA (CA:pmoves) (Rods (hc:a) (tc++b) [])

stack CB moves (Rods a b (hc:tc)) =
  let pmoves = stack CA moves (Rods [] [] tc)
  in stack AB (CB:pmoves) (Rods (tc++a) (hc:b) [])
 
getMoves :: Hanoi -> [Move]
getMoves = stack AC []

makeMoves :: Hanoi -> [Move] -> [Hanoi]
makeMoves hanoi = scanr move hanoi

split :: (a -> b) -> (a -> c) -> a -> (b, c)
split f g x = (f x, g x)

apply :: (a -> b, a) -> b
apply = uncurry ($)

play = reverse . apply . split makeMoves getMoves
playWith = play . getInitialHanoi

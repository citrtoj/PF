import Prelude hiding (Left, Right)

-- -------------- Arbori --------------

data Arb = Nil | Node Int Arb Arb deriving (Show, Eq)

t1 :: Arb
t1 = Node 1 (Node 2 (Node 4 Nil Nil) (Node 5 Nil Nil)) (Node 3 Nil Nil)

f :: Arb -> Int -> Arb
f (Node x (Node y (Node o a1 a2) a3) z) v = Node x (Node y (Node v a1 a2) a3) z

data Crumb = Left Int Arb | Right Int Arb deriving (Show, Eq)

type Trail = [ Crumb ]

type Zipper = (Arb, Trail)

goLeft :: Zipper -> Maybe Zipper
goLeft (Nil, _) = Nothing
goLeft (Node x a1 a2, t) = Just (a1, Left x a2 : t)

goRight :: Zipper -> Maybe Zipper
goRight (Nil, _) = Nothing
goRight (Node x a1 a2, t) = Just (a2, Right x a1 : t)

goUp :: Zipper -> Maybe Zipper
goUp (a, []) = Nothing
goUp (a, (Left x a2) : t) = Just (Node x a a2, t)
goUp (a, (Right x a1) : t) = Just (Node x a1 a, t)

change :: Int -> Zipper -> Maybe Zipper
change _ (Nil, _) = Nothing
change v (Node x a1 a2, t) = Just  (Node v a1 a2, t)

-- ghci> (Just (t1, [])) >>= goLeft >>= change 43 >>= goUp
-- Just (Node 1 (Node 43 (Node 4 Nil Nil) (Node 5 Nil Nil)) (Node 3 Nil Nil),[])

-- -------------- Liste --------------

data Crumb' = Forward' Int deriving (Show, Eq)

type Trail' = [Crumb']

type Zipper' = ([Int], Trail')

goFwd' :: Zipper' -> Maybe Zipper'
goFwd' ([], _) = Nothing
goFwd' (hd : tl, t) = Just (tl, Forward' hd : t)

goBwd' :: Zipper' -> Maybe Zipper'
goBwd' (_, []) = Nothing
goBwd' (l, Forward' x : t) = Just (x : l, t)

change' :: Int -> Zipper' -> Maybe Zipper'
change' v ([], _) = Nothing
change' v (hd : tl, t) = Just (v : tl, t)

-- ghci> (Just ([2, 4, 6, 8, 10], [])) >>= goFwd' >>= goFwd' >>= change' 43 >>= goBwd'           
-- Just ([4,43,8,10],[Forward' 2])
-- ghci>

-- -------------- Arbori generali --------------

data Tree'' = Nod Int [Tree''] deriving (Show, Eq)

data Dir = Down | Right' deriving (Show, Eq)

t = Nod 10 [Nod 3 [], Nod 4 [Nod 1 [], Nod 2 [], Nod 22 [], Nod 33[] ], Nod 5 [], Nod 6 []]

atPos :: [Tree''] -> [Dir] -> Int
atPos [] dirArray = error "No children"
atPos (Nod _ arr : otherChildren) (Down : d) = atPos arr d
atPos (currentChild : otherChildren) (Right' : d) = atPos otherChildren d
atPos (Nod val _: otherChildren) [] = val

-- ghci> atPos [t] [Down, Right', Down, Right', Right']
-- 22

data Crumb'' = Down'' Int [Tree''] | Right'' Tree'' deriving (Show, Eq) -- copii de stanga si de dreapta

type Trail'' = [ Crumb'' ]

type Zipper'' = ([Tree''], Trail'')

goDown'' :: ([Tree''], [Crumb'']) -> ([Tree''], [Crumb''])
goDown'' (((Nod v children@(child : otherChildren)) : others), crumbs) = (children, (Down'' v others : crumbs)) 
goDown'' ((Nod v [] : others), _) = error "No children"
goDown'' _ = error "Invalid zipper"

goRight'' :: ([Tree''], [Crumb'']) -> ([Tree''], [Crumb''])
goRight'' ((node : others), crumbs) = (others, Right'' node : crumbs)
goRight'' _ = error "Invalid zipper"

goBack'' :: ([Tree''], [Crumb'']) -> ([Tree''], [Crumb''])
goBack'' (tree, (Down'' value siblings) : crumbs) = ((Nod value tree) : siblings, crumbs)
goBack'' (tree, (Right'' leftSibling) : crumbs) = ((leftSibling : tree), crumbs)
goBack'' (tree, []) = error "Trail empty; cannot go back"

change''' :: Int -> ([Tree''], [Crumb'']) -> ([Tree''], [Crumb''])
change''' v ((Nod _ children : others), crumbs) = (Nod v children : others, crumbs)
change''' _ _ = error "Invalid zipper"
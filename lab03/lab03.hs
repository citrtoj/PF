-- pre-seminar

-- data Person = MkPerson
--               {
--                 name :: String
--                 , age :: Int
--                 , occupation :: String
--               } deriving (Show)

-- p = Person { "Sabina", 25, "profesor"}
-- name p



-- 1
data MobileDevice = Smartphone
                    | Laptop
                    | Tablet
                    deriving (Show)

-- 1.2
-- Ok, one module loaded.
-- ghci> :t Smartphone
-- Smartphone :: MobileDevice
-- ghci> :t Laptop    
-- Laptop :: MobileDevice
-- ghci> :t Tablet 
-- Tablet :: MobileDevice
-- ghci> 

-- 1.3
data Culori = Rosu
              | Albastru
              | RozBombon
              deriving (Show)

data MobileDevice' = Smartphone' Culori
                    | Laptop' Culori
                    | Tablet' Culori
                    deriving (Show)

-- ghci> :t Tablet' Rosu
-- Tablet' Rosu :: MobileDevice'
-- ghci> :t Smartphone' RozBombon
-- Smartphone' RozBombon :: MobileDevice'
-- ghci> 

descriere :: MobileDevice -> String
descriere Laptop = "Acesta este un laptop de culoare roz."
descriere Tablet = "Aceasta este o tableta mov."
descriere Smartphone = "Acesta este un telefon mobil."

descriereCuloareAux :: Culori -> String
descriereCuloareAux Rosu = "rosu"
descriereCuloareAux Albastru = "albastru"
descriereCuloareAux RozBombon = "roz bombon"

-- 1.4
descriere' :: MobileDevice' -> String
descriere' (Smartphone' culoare) = "Acesta este un Smartphone " ++ descriereCuloareAux culoare
descriere' (Laptop' culoare) = "Acesta este un Laptop " ++ descriereCuloareAux culoare
descriere' (Tablet' culoare) = "Aceasta este o Tableta " ++ descriereCuloareAux culoare

-- 2
-- am mers cu prima optiune
data Arb = Frunza | Nod Integer Arb Arb deriving (Show, Eq)

arb1 :: Arb
arb1 = Nod 5 (Nod 4 Frunza Frunza) (Nod 7 (Nod 6 Frunza Frunza) Frunza)

isBST :: Arb -> Bool
isBST Frunza = True
isBST (Nod y (Nod x arb1 arb2) (Nod z arb3 arb4)) = x < y && y < z && isBST arb1 && isBST arb2 && isBST arb3 && isBST arb4
isBST (Nod y (Nod x arb1 arb2) Frunza) = x < y && isBST arb1 && isBST arb2
isBST (Nod y Frunza (Nod z arb1 arb2)) = y < z && isBST arb1 && isBST arb2
isBST (Nod _ _ _) = True

search :: Arb -> Integer -> Bool
search Frunza _ = False
search (Nod x arb1 arb2) y = (x == y) || search arb1 y || search arb2 y

-- 2.4
insert :: Arb -> Integer -> Arb
insert Frunza x = Nod x Frunza Frunza
insert arb x | search arb x = arb -- enforces unique instances
insert (Nod y arb1 arb2) x | x < y = Nod y (insert arb1 x) arb2
                           | otherwise = Nod y arb1 (insert arb2 x)

minIntegerValue :: Integer
minIntegerValue = -99999999999

maxIntegerValue :: Integer
maxIntegerValue = 100000000000

maxBST :: Arb -> Integer
maxBST Frunza = minIntegerValue
maxBST (Nod x _ Frunza) = x
maxBST (Nod _ _ arb2) = maxBST arb2

minBST :: Arb -> Integer
minBST Frunza = maxIntegerValue
minBST (Nod x Frunza _) = x
minBST (Nod x arb1 _) = minBST arb1


removeMax :: Arb -> Arb
removeMax Frunza = Frunza
removeMax (Nod x (Nod y arb1 arb2) Frunza) = Nod y arb1 arb2
removeMax (Nod x arbSt (Nod y arb1 arb2)) = Nod x arbSt (removeMax (Nod y arb1 arb2))
removeMax (Nod x Frunza Frunza) = Frunza

removeRoot :: Arb -> Arb
removeRoot Frunza = Frunza
removeRoot (Nod x Frunza Frunza) = Frunza
removeRoot (Nod x arb Frunza) = arb
removeRoot (Nod x Frunza arb) = arb
removeRoot (Nod x arb1 arb2) = Nod (maxBST arb1) (removeMax arb1) arb2

remove :: Arb -> Integer -> Arb
remove (Nod y arb1 arb2) x | x < y = Nod y (remove arb1 x) arb2
                           | x > y = Nod y arb1 (remove arb2 x)
                           | otherwise = removeRoot (Nod y arb1 arb2)
remove _ _ = Frunza


preOrder :: Arb -> [Integer]
preOrder Frunza = []
preOrder (Nod x arb1 arb2) = x : preOrder arb1 ++ preOrder arb2

inOrder :: Arb -> [Integer]
inOrder Frunza = []
inOrder (Nod x arb1 arb2) = inOrder arb1 ++ [x] ++ inOrder arb2

postOrder :: Arb -> [Integer]
postOrder Frunza = []
postOrder (Nod x arb1 arb2) = postOrder arb1 ++ postOrder arb2 ++ [x]

-- 3.1
-- trebuie sa definesc cumva un environment .

data Boolean = Tru
             | Fals
             deriving (Show, Eq)

myOr :: Boolean -> Boolean -> Boolean
myOr Fals Fals = Fals
myOr _ _       = Tru

myAnd :: Boolean -> Boolean -> Boolean
myAnd Tru Tru = Tru
myAnd _ _     = Fals

myNot :: Boolean -> Boolean
myNot Tru = Fals
myNot Fals = Tru

type Env = String -> Boolean

env1 :: Env
env1 "n" = Tru
env1 "i" = Fals
env1 _ = Fals

updateEnv :: Env -> String -> Boolean -> Env
-- schimbat din lambda in ceva de genul pentru ca
-- imi comenta IDE-ul ca "redundant lambda"
-- desi mna am inteles scopul
updateEnv env x b y = if y == x then b else env y

data BExp = Var String
          | Const Boolean
          | And BExp BExp
          | Or BExp BExp
          | Not BExp
          deriving (Show)


eval :: BExp -> Env -> Boolean
eval (Var s) env = env s
eval (Const b) env = b
eval (And b1 b2) env = myAnd (eval b1 env) (eval b2 env)
eval (Or b1 b2) env = myOr (eval b1 env) (eval b2 env)
eval (Not b) env = myNot (eval b env)

egal :: BExp -> BExp -> Bool
egal (Const b1) (Const b2) = b1 == b2
egal (Var v1) (Var v2) = v1 == v2
egal (Not b1) (Not b2) = egal b1 b2
egal (And b11 b12) (And b21 b22) = egal b11 b21 && egal b12 b22
egal (Or b11 b12) (Or b21 b22) = egal b11 b21 && egal b12 b22
egal _ _ = False

simpl :: BExp -> BExp
simpl (Var s) = Var s
simpl (Const b) = Const b
simpl (And e1 e2) = let e1' = simpl e1 in
                    let e2' = simpl e2 in
                        if egal e1' (Const Fals) || egal e2' (Const Fals) then
                            Const Fals
                        else if egal e1' (Const Tru) && egal e2' (Const Tru) then
                            Const Tru
                        else And e1' e2'
simpl (Or e1 e2) = let e1' = simpl e1 in
                    let e2' = simpl e2 in
                        if egal e1' (Const Tru) || egal e2' (Const Tru) then
                            Const Tru
                        else if egal e1' (Const Fals) && egal e2' (Const Fals) then
                            Const Fals
                        else Or e1' e2'
simpl (Not e1) = Not (simpl e1)

-- functiile NU au fost testate dar ar trb sa fie ok..?
checkIfNonNegatedLiteral :: BExp -> Bool
checkIfNonNegatedLiteral (Const c) = True
checkIfNonNegatedLiteral (Var x)   = True
checkIfNonNegatedLiteral _         = False

checkIfLiteral :: BExp -> Bool
checkIfLiteral (Const c) = True
checkIfLiteral (Var x)   = True
checkIfLiteral (Not e1)  = checkIfNonNegatedLiteral e1
checkIfLiteral _         = False

checkIfClause :: BExp -> Bool
checkIfClause (Const c) = True
checkIfClause (Var x) = True
checkIfClause (Or e1 e2) = checkIfClause e1 && checkIfLiteral e2

checkFNC :: BExp -> Bool
checkFNC (Const c) = True
checkFNC (Var x)   = True
checkFNC (And e1 e2) = checkFNC e1 && checkIfClause e2 
checkFNC e = checkIfClause e

-- -- aplica efectiv regula daca gaseste vreuna 
-- applyFNCHelper :: BExp -> BExp
-- -- TODO
-- nu am mai apucat sa o fac. dar se aplica regulile din cursul de logica. cred.

-- applyFNC :: BExp -> Bool
-- applyFNC e | checkFNC e == False = applyFNC (applyFNCHelper e)
--            | otherwise           = e
import Data.Set(Set,size,elems,fromList,unions)
import qualified Data.Set

data Val = Orig | Add Val Val | Mul Val Val | Neg Val | Recip Val
    deriving (Eq,Ord,Show)

canonical :: Val -> Val
canonical Orig = Orig
canonical (Add (Add Orig a) b) = Add Orig (canonical (Add a b))
canonical (Add (Add a Orig) b) = Add Orig (canonical (Add a b))
canonical (Add a (Add Orig b)) = Add Orig (canonical (Add a b))
canonical (Add a (Add b Orig)) = Add Orig (canonical (Add a b))
canonical (Add (Neg a) (Neg b)) = Neg (canonical (Add a b))
canonical (Add a b) = Add (min ca cb) (max ca cb)
  where (ca,cb) = (canonical a,canonical b)
canonical (Mul (Mul Orig a) b) = Mul Orig (canonical (Mul a b))
canonical (Mul (Mul a Orig) b) = Mul Orig (canonical (Mul a b))
canonical (Mul a (Mul Orig b)) = Mul Orig (canonical (Mul a b))
canonical (Mul a (Mul b Orig)) = Mul Orig (canonical (Mul a b))
canonical (Mul (Recip a) (Recip b)) = Recip (canonical (Mul a b))
canonical (Mul a b) = Mul (min ca cb) (max ca cb)
  where (ca,cb) = (canonical a,canonical b)
canonical (Neg (Add (Neg a) (Neg b))) = canonical (Add a b)
canonical (Neg (Add (Neg a) b)) = canonical (Add a (Neg b))
canonical (Neg (Add a (Neg b))) = canonical (Add b (Neg a))
canonical (Neg (Neg a)) = canonical a
canonical (Neg a) = Neg (canonical a)
canonical (Recip (Mul (Recip a) (Recip b))) = canonical (Mul a b)
canonical (Recip (Mul (Recip a) b)) = canonical (Mul a (Recip b))
canonical (Recip (Mul a (Recip b))) = canonical (Mul b (Recip a))
canonical (Recip (Recip a)) = canonical a
canonical (Recip a) = Recip (canonical a)

commuters :: Val -> [Int]
commuters Orig = []
commuters (Add Orig a) = addCommuters 1 a
  where
    addCommuters n Orig = [n+1]
    addCommuters n (Add Orig a) = addCommuters (n+1) a
    addCommuters n a = n : commuters a
commuters (Add a b) = commuters a ++ commuters b
commuters (Mul Orig a) = mulCommuters 1 a
  where
    mulCommuters n Orig = [n+1]
    mulCommuters n (Mul Orig a) = mulCommuters (n+1) a
    mulCommuters n a = n : commuters a
commuters (Mul a b) = commuters a ++ commuters b
commuters (Neg a) = commuters a
commuters (Recip a) = commuters a

origs :: Val -> Int
origs Orig = 1
origs (Add a b) = origs a + origs b
origs (Mul a b) = origs a + origs b
origs (Neg a) = origs a
origs (Recip a) = origs a

one :: Set Val
one = fromList [Orig]

combos :: Set Val -> Set Val -> Set Val
combos as bs = Data.Set.map canonical $ fromList $ concat [[Add a b,Add (Neg a) b,Add a (Neg b),Mul a b,Mul (Recip a) b,Mul a (Recip b)] | a <- elems as, b <- elems bs]

two :: Set Val
two = combos one one

three :: Set Val
three = combos one two

four :: Set Val
four = unions [combos one three,combos two two]

five :: Set Val
five = unions [combos one four,combos two three]

six :: Set Val
six = unions [combos one five,combos two four,combos three three]

choose :: Int -> Int -> Int
choose n k = product [k+1..n] `div` product [1..n-k]

fac :: Int -> Int
fac n = product [1..n]

distincts :: Int -> Val -> Int
distincts totalOrigs val = choose totalOrigs (origs val) * fac (origs val) `div` product (map fac (commuters val))

main :: IO ()
main = print $ sum $ map (sum . map (distincts 6) . elems) [one,two,three,four,five,six]

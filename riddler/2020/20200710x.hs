import Data.List(permutations)
import Data.Ratio(denominator,numerator,(%))

data Op = Op String (Rational -> Rational -> Maybe Rational)

instance Show Op where
  show (Op name _) = name

apply :: Op -> Maybe Rational -> Maybe Rational -> Maybe Rational
apply _ Nothing _ = Nothing
apply _ _ Nothing = Nothing
apply (Op _ op) (Just a) (Just b) = op a b

ops :: [Op]
ops = [Op "+" (tryOp (+)),Op "-" (tryOp (-)),Op "*" (tryOp (*)),Op "/" tryDiv,Op "^" tryExp,Op "f-" (tryOp (flip (-))),Op "f/" (flip tryDiv),Op "f^" (flip tryExp)]
  where
    tryOp :: (Rational -> Rational -> Rational)  -> Rational -> Rational -> Maybe Rational
    tryOp op = \ a b -> Just (op a b)
    tryDiv :: Rational -> Rational -> Maybe Rational
    tryDiv a b
      | numerator b == 0 = Nothing
      | otherwise = Just (a / b)
    tryExp :: Rational -> Rational -> Maybe Rational
    tryExp a b
      | denominator b /= 1 = Nothing
      | numerator b < 0 && numerator a == 0 = Nothing
      | numerator b < 0 = fmap ((1 % 1) /) (pow a (- numerator b))
      | otherwise = pow a (numerator b)
    pow :: Rational -> Integer -> Maybe Rational
    pow a b
      | b > 100 = Nothing
      | otherwise = Just (product (take (fromIntegral b) (repeat a)))

applySeq :: (Op,Op,Op) -> (Maybe Rational,Maybe Rational,Maybe Rational,Maybe Rational) -> Maybe Rational
applySeq (x,y,z) (a,b,c,d) = apply z (apply y (apply x a b) c) d

applyPairs :: (Op,Op,Op) -> (Maybe Rational,Maybe Rational,Maybe Rational,Maybe Rational) -> Maybe Rational
applyPairs (x,y,z) (a,b,c,d) = apply z (apply x a b) (apply y c d)

search :: [((Op,Op,Op),(Maybe Rational,Maybe Rational,Maybe Rational,Maybe Rational),Maybe Rational,Maybe Rational)]
search = [(opseq,numseq,applySeq opseq numseq,applyPairs opseq numseq) | opseq <- opseqs, numseq <- numseqs,applySeq opseq numseq == Just (24%1) || applyPairs opseq numseq == Just (24%1)]
  where
    opseqs = [(x,y,z) | x <- ops, y <- ops, z <- ops]
    numseqs = [(a,b,c,d) | [a,b,c,d] <- permutations [Just (2%1),Just (3%1),Just (3%1),Just (4%1)]]

main :: IO ()
main = mapM_ print search

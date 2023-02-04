import Data.Char(isLower)
import Data.Set(Set,difference,fromList,member,size,toList,union)

parse :: String -> (String,[(String,String)])
parse s = parseReplacements [] (lines s)
  where
    parseReplacements replacements [] = error "bad input"
    parseReplacements replacements ["",start] = (start,replacements)
    parseReplacements replacements (r:rs) = parseReplacements ((src,dest):replacements) rs
      where
        (src:_:dest:_) = words r

replace :: (String,[(String,String)]) -> [String]
replace (s,rules)
  | null s = []
  | otherwise = map (head s:) (replace (tail s,rules)) ++ concatMap replace1 rules
  where
    replace1 (src,dest)
      | take (length src) s == src = [dest ++ drop (length src) s]
      | otherwise = []

test :: ()
test
  | (size $ fromList $ replace ("HOH",rules)) /= 4 = error "a"
  | (size $ fromList $ replace ("HOHOHO",rules)) /= 7 = error "b"
  | otherwise = ()
  where
    (_,rules) = parse "H => HO\nH => OH\nO => HH\n\nHOH"

part1 :: IO Int
part1 = fmap (size . fromList . replace . parse) (readFile "input/19.txt")

-- For part 2, brute force does not work.  There are too many replacements.
--
-- Put the replacement rules of my input in the code.  I don't know if this
-- works for other inputs.
--
-- Try to reduce the target molecule with reverse replacements.
--
-- Note that Ar, Y, Rn, and CRn are never replaced, and Ar is always at the
-- end of a replacement, which also always contain a Rn, so it's possible
-- to consider the nested Rn-Ar pairs recursively.  The Ys always come
-- between Rn and Ar, which also can be exploited by divide-and-conquer.
--
-- And between the Rn, Y, Ar must always come from one of F, Mg, Al.

-- Once Si and Th are created, they can never disappear.  Si can only generate
-- Ca to its left and Th can only generate Ca to its right.

-- Al, F, Mg can only come at the end of a replacement (or before Ar or Y),
-- so whatever immediately follows them must be the start of a replacement.
-- H, N, O can only come at the start of a replacement, so whatever immediately
-- precedes them must the end of a replacement.

data Molecule = E | Al | B | Ca | F | H | Mg | N | O | P | Si | Th | Ti
              | CRn_Ar [Molecule]
              | CRn_Y_Ar [Molecule] [Molecule]
              | CRn_Y_Y_Ar [Molecule] [Molecule] [Molecule]
              | Rn_Ar [Molecule] [Molecule]
              | Rn_Y_Ar [Molecule] [Molecule] [Molecule]
              deriving (Eq,Show)

parseMolecule :: String -> [Molecule]
parseMolecule s = fst (p [] s)
  where
    p revPreds s
      | null s = (reverse revPreds,"")
      | take 3 s == "CRn" = pCRn revPreds (drop 3 s)
      | take 2 s == "Al" = p (Al:revPreds) (drop 2 s)
      | take 2 s == "Ca" = p (Ca:revPreds) (drop 2 s)
      | take 2 s == "Mg" = p (Mg:revPreds) (drop 2 s)
      | take 2 s == "Si" = p (Si:revPreds) (drop 2 s)
      | take 2 s == "Th" = p (Th:revPreds) (drop 2 s)
      | take 2 s == "Ti" = p (Ti:revPreds) (drop 2 s)
      | take 2 s == "Rn" = pRn revPreds (drop 2 s)
      | take 2 s == "Ar" = (reverse revPreds,s)
      | take 1 s == "Y" = (reverse revPreds,s)
      | take 1 s == "B" = p (B:revPreds) (drop 1 s)
      | take 1 s == "F" = p (F:revPreds) (drop 1 s)
      | take 1 s == "H" = p (H:revPreds) (drop 1 s)
      | take 1 s == "N" = p (N:revPreds) (drop 1 s)
      | take 1 s == "O" = p (O:revPreds) (drop 1 s)
      | take 1 s == "P" = p (P:revPreds) (drop 1 s)
      | otherwise = error ("bad molecule:" ++ s)
    pCRn revPreds s
      | take 2 rest == "Ar" = p (CRn_Ar molecules:revPreds) (drop 2 rest)
      | take 1 rest == "Y" = pCRnY revPreds molecules (drop 1 rest)
      | otherwise = error ("bad molecule:" ++ s)
      where (molecules,rest) = p [] s
    pCRnY revPreds mols1 s
      | take 2 rest == "Ar" = p (CRn_Y_Ar mols1 molecules:revPreds) (drop 2 rest)
      | take 1 rest == "Y" = pCRnYY revPreds mols1 molecules (drop 1 rest)
      | otherwise = error ("bad molecule:" ++ s)
      where (molecules,rest) = p [] s
    pCRnYY revPreds mols1 mols2 s
      | take 2 rest == "Ar" = p (CRn_Y_Y_Ar mols1 mols2 molecules:revPreds) (drop 2 rest)
      | otherwise = error ("bad molecule:" ++ s)
      where (molecules,rest) = p [] s
    pRn revPreds s
      | take 2 rest == "Ar" = p [Rn_Ar (reverse revPreds) molecules] (drop 2 rest)
      | take 1 rest == "Y" = pRnY revPreds molecules (drop 1 rest)
      | otherwise = error ("bad molecule:" ++ s)
      where (molecules,rest) = p [] s
    pRnY revPreds mols1 s
      | take 2 rest == "Ar" = p [Rn_Y_Ar (reverse revPreds) mols1 molecules] (drop 2 rest)
      | otherwise = error ("bad molecule:" ++ s)
      where (molecules,rest) = p [] s

showMolecule :: Molecule -> String
showMolecule (CRn_Ar m) = "CRn" ++ concatMap showMolecule m ++ "Ar"
showMolecule (CRn_Y_Ar m1 m2) = "CRn" ++ concatMap showMolecule m1 ++ "Y" ++ concatMap showMolecule m2 ++ "Ar"
showMolecule (CRn_Y_Y_Ar m1 m2 m3) = "CRn" ++ concatMap showMolecule m1 ++ "Y" ++ concatMap showMolecule m2 ++ "Y" ++ concatMap showMolecule m3 ++ "Ar"
showMolecule (Rn_Ar pred m) = concatMap showMolecule pred ++ "Rn" ++ concatMap showMolecule m ++ "Ar"
showMolecule (Rn_Y_Ar pred m1 m2) = concatMap showMolecule pred ++ "Rn" ++ concatMap showMolecule m1 ++ "Y" ++ concatMap showMolecule m2 ++ "Ar"
showMolecule m = show m

reduce :: [Molecule] -> (Int,[Molecule])
reduce [] = (0,[])
reduce mols | steps > 0 = (steps,reduced)
  where (steps,reduced) = reduceRecursive mols
reduce (mol:mols) | steps > 0 = (steps,mol:reduced)
  where (steps,reduced) = reduce mols
reduce (Th:F:mols) = (1,Al:mols)
reduce (B:Ca:mols) = (1,B:mols)
reduce (Ti:B:mols) = (1,B:mols)
reduce (Ca:Ca:mols) = (1,Ca:mols)
reduce (P:B:mols) = (1,Ca:mols)
reduce (Si:Th:mols) = (1,Ca:mols)
reduce (Ca:F:mols) = (1,F:mols)
reduce (P:Mg:mols) = (1,F:mols)
reduce (Si:Al:mols) = (1,F:mols)
reduce (H:Ca:mols) = (1,H:mols)
reduce (N:Th:mols) = (1,H:mols)
reduce (O:B:mols) = (1,H:mols)
reduce (B:F:mols) = (1,Mg:mols)
reduce (Ti:Mg:mols) = (1,Mg:mols)
reduce (H:Si:mols) = (1,N:mols)
reduce (H:P:mols) = (1,O:mols)
reduce (O:Ti:mols) = (1,O:mols)
reduce (Ca:P:mols) = (1,P:mols)
reduce (P:Ti:mols) = (1,P:mols)
reduce (Ca:Si:mols) = (1,Si:mols)
reduce (Th:Ca:mols) = (1,Th:mols)
reduce (B:P:mols) = (1,Ti:mols)
reduce (Ti:Ti:mols) = (1,Ti:mols)
reduce (H:F:mols) = (1,E:mols)
reduce (N:Al:mols) = (1,E:mols)
reduce (O:Mg:mols) = (1,E:mols)
reduce (mol:mols) = let (n,reduced) = reduce mols in (n,mol:reduced)

reduceRecursive :: [Molecule] -> (Int,[Molecule])
reduceRecursive mols = (sum (map fst reduced),concatMap snd reduced)
  where reduced = map reduce1 mols

reduce1 :: Molecule -> (Int,[Molecule])
reduce1 (CRn_Ar [Al]) = (1,[H])
reduce1 (CRn_Ar [F]) = (1,[N])
reduce1 (CRn_Ar [Mg]) = (1,[O])
reduce1 (CRn_Ar mols) = (steps,[CRn_Ar reduced])
  where (steps,reduced) = reduce mols
reduce1 (CRn_Y_Ar [F] [Mg]) = (1,[H])
reduce1 (CRn_Y_Ar [Mg] [F]) = (1,[H])
reduce1 (CRn_Y_Ar [F] [F]) = (1,[O])
reduce1 (CRn_Y_Ar mols1 mols2) = (steps1+steps2,[CRn_Y_Ar reduced1 reduced2])
  where (steps1,reduced1) = reduce mols1
        (steps2,reduced2) = reduce mols2
reduce1 (CRn_Y_Y_Ar [F] [F] [F]) = (1,[H])
reduce1 (CRn_Y_Y_Ar mols1 mols2 mols3) = (steps1+steps2+steps3,[CRn_Y_Y_Ar reduced1 reduced2 reduced3])
  where (steps1,reduced1) = reduce mols1
        (steps2,reduced2) = reduce mols2
        (steps3,reduced3) = reduce mols3
reduce1 mol@(Rn_Ar preds [F])
  | last preds == Th = (1,reverse (Al:tail (reverse preds)))
  | last preds == Ti = (1,reverse (B:tail (reverse preds)))
  | last preds == P = (1,reverse (Ca:tail (reverse preds)))
  | last preds == O = (1,reverse (H:tail (reverse preds)))
  | last preds == N = (1,reverse (O:tail (reverse preds)))
  | last preds == Si = (1,reverse (P:tail (reverse preds)))
  | otherwise = (psteps,[Rn_Ar reducedPreds [F]])
  where (psteps,reducedPreds) = reduceRecursive preds
reduce1 mol@(Rn_Ar preds [Mg]) 
  | last preds == Si = (1,reverse (Ca:tail (reverse preds)))
  | last preds == N = (1,reverse (H:tail (reverse preds)))
  | otherwise = (psteps,[Rn_Ar reducedPreds [Mg]])
  where (psteps,reducedPreds) = reduceRecursive preds
reduce1 (Rn_Ar preds mols) = (psteps+steps,[Rn_Ar reducedPreds reduced])
  where (steps,reduced) = reduce mols
        (psteps,reducedPreds) = reduceRecursive preds
reduce1 (Rn_Y_Ar preds [F] [F])
  | last preds == Si = (1,reverse (Ca:tail (reverse preds)))
  | last preds == N = (1,reverse (H:tail (reverse preds)))
reduce1 (Rn_Y_Ar preds mols1 mols2) = (psteps+steps1+steps2,[Rn_Y_Ar reducedPreds reduced1 reduced2])
  where (steps1,reduced1) = reduce mols1
        (steps2,reduced2) = reduce mols2
        (psteps,reducedPreds) = reduceRecursive preds
reduce1 mol = (0,[mol])

reduceFully :: [Molecule] -> (Int,Molecule)
reduceFully mols = r 0 (reduce mols)
  where
    r n (steps,[mol]) = (n+steps,mol)
    r n (steps,reduced) = r (n+steps) (reduce reduced)

part2 :: IO Int
part2 = fmap (fst . reduceFully . parseMolecule . fst . parse) (readFile "input/19.txt")

-- I notice that the replacements in my input always replace an atom with
-- either two atoms or an atom and a Rn compound.  Ignoring the Rn compounds,
-- the number of replacements is the number of atoms, where the last atom
-- counts as the replacement of the electron.
--
-- Since the Rn compounds are always
-- Rn + 1 atom + (zero or more Y + 1 atom) + Ar
-- those can be accounted for by counting atoms, then subtracting double the
-- count of Rn and Y atoms.

-- Somehow, this doesn't quite work and overcounts by 1 for my input.

alternatePart2count :: String -> Int
alternatePart2count "" = 0
alternatePart2count ('R':'n':s) = -1 + alternatePart2count s
alternatePart2count ('Y':s) = -1 + alternatePart2count s
alternatePart2count (_:s) = 1 + alternatePart2count (dropWhile isLower s)

alternatePart2 :: IO Int
alternatePart2 = fmap (alternatePart2count . fst . parse) (readFile "input/19.txt")

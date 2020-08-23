import Data.Set(member,fromList)

removes :: String -> [(String,String)]
removes word = r "" word
  where
    r p (l1:l2:l3:ls) = (reverse p++ls,[l1,l2,l3]) : r (l1:p) (l2:l3:ls)
    r _ _ = []

adds :: String -> String -> [String]
adds word part = a "" word
  where
    a p (l:ls) = (reverse p ++ part ++ (l:ls)) : a (l:p) ls
    a p [] = []

combos :: String -> String -> [String]
combos w1 w2 = concat [[a++r | a <- adds w1 part] | (r,part) <- removes w2]

answers :: [String] -> (String -> Bool) -> [String]
answers words ok = concat [[w1++" "++w2++" "++c | c <- combos w1 w2, ok c] | w1 <- words, w2 <- words, length w1 + length w2 == 10, w1 /= w2]

parts :: [String]
parts = [
    "head","heart","brain","fist","hand","knee","nose","mouth","tongue",
    "finger","foot","pelvis","spine","skull","skin","elbow","knuckle",
    "tooth","throat","eyelid","sole","lung","stomach","nail","nostril",
    "eyelash","trachea","uvea","iris","eyebrow","appendix","colon",
    "toenail","torso","thigh","calf","heel","kidney","spleen","sinus",
    "liver","thumb","palm","navel","nipple","earlobe","cranium","hips",
    "femur","ulna","vein","artery","aorta","foreskin","pupil","bone",
    "eye","ear","forehead"
    ]

main :: IO ()
main = do
    set <- fmap (fromList . filter ((== 10) . length) . lines) (readFile "/usr/share/dict/words")
    mapM_ putStrLn (answers parts (flip member set))

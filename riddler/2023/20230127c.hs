winProb :: Rational
winProb = 1
    - 2/26*sum [loseProbX____ ch | ch <- [0..4]]
--    0/26*sum [loseProb____X ch | ch <- [21..25]] -- by symmetry

    - 2/26*sum [loseProb_X___ ch | ch <- [5..9]]
--    0/26*sum [loseProb___X_ ch | ch <- [16..20]] -- by symmetry

    - 2/26*sum [loseProb__X__ ch | ch <- [10..12]]
--    0/26*sum [loseProb__X__ ch | ch <- [13..15]] -- by symmetry

loseProbX____ :: Rational -> Rational
loseProbX____ ch =
      ch/25
    + 1/25*sum [
        if chX < middle
          then
            if chX-ch < middle-chX
              then loseProbXX___ chX
              else loseProbX_X__ ch chX
          else
            if chX-middle < 26-chX
              then loseProbX__X_ ch chX
              else loseProbX___X ch chX
      | chX <- [ch+1..25]]
  where
    middle = fromIntegral (floor ((ch+26)/2))

loseProb_X___ :: Rational -> Rational
loseProb_X___ ch =
      ch/25*loseProbXX___ ch
    + 1/25*sum [
        loseProb_XX__ ch chX
      | chX <- [ch+1..ch+n]]
    + 1/25*sum [
        loseProb_X_X_ ch chX
      | chX <- [ch+n+1..25-n]]
    + 1/25*sum [
        loseProbX__X_ (25-chX) (25-ch)
      | chX <- [26-n..25]]
  where
    n = fromIntegral ((25 - floor ch) `div` 3)

loseProb__X__ :: Rational -> Rational
loseProb__X__ ch =
      1/25*sum [
          if chX < ch-chX
            then loseProbX_X__ chX ch
            else loseProb_XX__ chX ch
        | chX <- [0..ch-1]]
    + 1/25*sum [
          if chX-ch < 25-chX
            then loseProb_XX__ (25-chX) (25-ch)
            else loseProbX_X__ (25-chX) (25-ch)
        | chX <- [ch+1..25]]


loseProbXX___ :: Rational -> Rational
loseProbXX___ ch =
      (ch-1)/24
    + 1/24*sum [
        loseProbXXX__ chX
      | chX <- [ch+1..ch+n]]
    + 1/24*sum [
        loseProbXX_X_ ch chX
      | chX <- [ch+n+1..25-n]]
    + 1/24*sum [
        loseProbXX__X ch chX
      | chX <- [26-n..25]]
  where
    n = fromIntegral ((25 - floor ch) `div` 3)

loseProbX_X__ :: Rational -> Rational -> Rational
loseProbX_X__ ch1 ch2 =
      ch1/24
    + (ch2-1-ch1)/24*loseProbXXX__ ch2
    + 1/24*sum [
        if ch3-ch2 < 25-ch3
          then loseProbX_XX_ ch1 ch2 ch3
          else loseProbX_X_X ch1 ch2 ch3
      | ch3 <- [ch2+1..25]]

loseProbX__X_ :: Rational -> Rational -> Rational
loseProbX__X_ ch1 ch2 =
      ch1/24
    + 1/24*sum [
        if chX-ch1 < ch2-chX
          then loseProbXX_X_ chX ch2
          else loseProbX_XX_ ch1 chX ch2
      | chX <- [ch1+1..ch2-1]]
    + (25-ch2)/24*loseProbXX__X (25-ch2) (25-ch1)

loseProbX___X :: Rational -> Rational -> Rational
loseProbX___X ch1 ch2 =
      ch1/24
    + 1/24*sum [
        loseProbXX__X chX ch2
      | chX <- [ch1+1..ch1+n]]
    + 1/24*sum [
        loseProbX_X_X ch1 chX ch2
      | chX <- [ch1+n+1..ch2-n-1]]
    + 1/24*sum [
        loseProbXX__X (25-chX) (25-ch1)
      | chX <- [ch2-n..ch2-1]]
    + (25-ch2)/24
  where
    n = fromIntegral ((floor ch2 - 1 - floor ch1) `div` 3)

loseProb_XX__ :: Rational -> Rational -> Rational
loseProb_XX__ ch1 ch2 =
      ch1/24*loseProbXXX__ ch2
    + (ch2-1-ch1)/24
    + 1/24*sum [
        if ch3-ch2 < 25-ch3
          then loseProb_XXX_ ch1 ch3
          else loseProbX_XX_ (25-ch3) (25-ch2) (25-ch1)
      | ch3 <- [ch2+1..25]]

loseProb_X_X_ :: Rational -> Rational -> Rational
loseProb_X_X_ ch1 ch2 =
      ch1/24*loseProbXX_X_ ch1 ch2
    + (ch2-1-ch1)/24*loseProb_XXX_ ch1 ch2
    + (25-ch2)/24*loseProbXX_X_ (25-ch2) (25-ch1)


loseProbXXX__ :: Rational -> Rational
loseProbXXX__ ch =
      (ch-2)/23
    + 1/23*sum [
        if chX-ch < 25-chX
          then loseProbLast chX 25
          else loseProbLast ch chX
      | chX <- [ch+1..25]]

loseProbXX_X_ :: Rational -> Rational -> Rational
loseProbXX_X_ ch1 ch2 =
      (ch1-1)/23
    + (ch2-1-ch1)/23*loseProbLast ch2 26
    + (25-ch2)/23*loseProbLast ch1 ch2

loseProbXX__X :: Rational -> Rational -> Rational
loseProbXX__X ch1 ch2 =
      (ch1-1)/23
    + 1/23*sum [
        if chX-ch1 < ch2-chX
          then loseProbLast chX ch2
          else loseProbLast ch1 chX
      | chX <- [ch1+1..ch2-1]]
    + (25-ch2)/23

loseProbX_XX_ :: Rational -> Rational -> Rational -> Rational
loseProbX_XX_ ch1 ch2 ch3 =
      ch1/23
    + (ch2-1-ch1)/23*loseProbLast ch3 26
    + (ch3-1-ch2)/23
    + (25-ch3)/23*loseProbLast ch1 ch2

loseProbX_X_X :: Rational -> Rational -> Rational -> Rational
loseProbX_X_X ch1 ch2 ch3 =
      ch1/23
    + (ch2-1-ch1)/23*loseProbLast ch2 ch3
    + (ch3-1-ch2)/23*loseProbLast ch1 ch2
    + (25-ch3)/23

loseProb_XXX_ :: Rational -> Rational -> Rational
loseProb_XXX_ ch1 ch2 =
      ch1/23*loseProbLast ch2 26
    + (ch2-1-ch1-1)/23
    + (25-ch2)/23*loseProbLast (-1) ch1


loseProbLast :: Rational -> Rational -> Rational
loseProbLast ch1 ch2 = 1 - (ch2-1-ch1)/22

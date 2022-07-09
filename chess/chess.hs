import Data.Map(Map,delete,fromList,insert,(!))
import qualified Data.Map
import System.Environment(getArgs)

data Row = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 deriving (Bounded,Enum,Eq,Ord,Show)

data Column = CA | CB | CC | CD | CE | CF | CG | CH deriving (Bounded,Enum,Eq,Ord,Show)

type Square = (Column,Row)

data Side = White | Black deriving (Eq,Show)

data Piece = R | N | B | Q | K | P deriving (Eq,Show)

type Board = Map Square (Side,Piece)

type State = (Side,Board)

type Move = (Square,Square,String) -- (source,destination,extra description)

mated :: State -> Bool
mated = null . legalMoves

mateIn1 :: State -> [(Move,State)]
mateIn1 = filter (mated . snd) . legalMoves

matedIn1 :: State -> Bool
matedIn1 = all (not . null . mateIn1 . snd) . legalMoves

mateIn2 :: State -> [(Move,State)]
mateIn2 = filter (matedIn1 . snd) . legalMoves

legalMoves :: State -> [(Move,State)]
legalMoves (side,board) = filter (not . inCheck . snd . snd) (moves side board)
  where
    moves side board = concat [movesFrom side (col,row) board | col <- [minBound..maxBound], row <- [minBound..maxBound]]
    inCheck board = or [isK (col,row) && isAttacked (col,row) | col <- [minBound..maxBound], row <- [minBound..maxBound]]
      where
        isK square = maybe False (\(s,p) -> s == side && p == K) (Data.Map.lookup square board)
        enemy = if side == White then Black else White
        isAttacked square = any (isTaken square) (moves enemy board)
        isTaken square (_,(_,board)) = Just (side,K) /= Data.Map.lookup square board

movesFrom :: Side -> Square -> Board -> [(Move,State)]
movesFrom side square board = maybe [] pieceMoves (Data.Map.lookup square board)
  where
    clear square = maybe True (const False) (Data.Map.lookup square board)
    enemy square = maybe False ((/= side) . fst) (Data.Map.lookup square board)
    friend square = not (clear square) && not (enemy square)
    -- omit en passant and castling since they require move history
    pieceMoves (s,piece)
      | s /= side = []
      | piece == R = movesR
      | piece == N = movesN
      | piece == B = movesB
      | piece == Q = movesQ
      | piece == K = movesK
      | piece == P = movesP (if side == White then (R2,R8,succ) else (R7,R1,pred))
      where
        (c,r) = square
        movesR = moveLine square (Just CA) Nothing pred id ++
                 moveLine square (Just CH) Nothing succ id ++
                 moveLine square Nothing (Just R1) id pred ++
                 moveLine square Nothing (Just R8) id succ
        movesB = moveLine square (Just CA) (Just R1) pred pred ++
                 moveLine square (Just CH) (Just R1) succ pred ++
                 moveLine square (Just CA) (Just R8) pred succ ++
                 moveLine square (Just CH) (Just R8) succ succ
        movesQ = movesR ++ movesB
        moveLine :: Square -> Maybe Column -> Maybe Row -> (Column -> Column) -> (Row -> Row) -> [(Move,State)]
        moveLine dest@(col,row) climit rlimit cadv radv
          | col == c && row == r =
              if Just col == climit || Just row == rlimit then []
              else moveLine (cadv col,radv row) climit rlimit cadv radv
          | friend dest = []
          | enemy dest || Just col == climit || Just row == rlimit = to dest piece
          | otherwise = to dest piece ++ moveLine (cadv col,radv row) climit rlimit cadv radv
        movesN =
            (if c /= CH && r /= R8 && r /= R7 then to (succ c,succ (succ r)) N else []) ++
            (if c /= CH && c /= CG && r /= R8 then to (succ (succ c),succ r) N else []) ++
            (if c /= CH && c /= CG && r /= R1 then to (succ (succ c),pred r) N else []) ++
            (if c /= CH && r /= R1 && r /= R2 then to (succ c,pred (pred r)) N else []) ++
            (if c /= CA && r /= R1 && r /= R2 then to (pred c,pred (pred r)) N else []) ++
            (if c /= CA && c /= CB && r /= R1 then to (pred (pred c),pred r) N else []) ++
            (if c /= CA && c /= CB && r /= R8 then to (pred (pred c),succ r) N else []) ++
            (if c /= CA && r /= R8 && r /= R7 then to (pred c,succ (succ r)) N else [])
        movesK =
            (if c /= minBound && r /= minBound then to (pred c,pred r) K else []) ++
            (if c /= minBound then to (pred c,r) K else []) ++
            (if c /= minBound && r /= maxBound then to (pred c,succ r) K else []) ++
            (if c /= maxBound && r /= minBound then to (succ c,pred r) K else []) ++
            (if c /= maxBound then to (succ c,r) K else []) ++
            (if c /= maxBound && r /= maxBound then to (succ c,succ r) K else []) ++
            (if r /= minBound then to (c,pred r) K else []) ++
            (if r /= maxBound then to (c,succ r) K else [])
        movesP :: (Row,Row,(Row -> Row)) -> [(Move,State)]
        movesP (start,end,forward) =
            (if r == start && clear (c,forward r) && clear (c,forward (forward r)) then to (c,forward (forward r)) P else []) ++
            (if clear (c,forward r) then pawnTo (c,forward r) else []) ++
            (if c /= minBound && enemy (pred c,forward r) then pawnTo (pred c,forward r) else []) ++
            (if c /= maxBound && enemy (succ c,forward r) then pawnTo (succ c,forward r) else [])
          where
            pawnTo dest@(_,destrow)
              | destrow /= end = to dest P
              | otherwise = to dest Q ++ to dest N
        to dest movePiece
          | friend dest = []
          | otherwise = [((square,dest,show piece ++ takeMsg ++ promoteMsg),(if side == White then Black else White,delete square (insert dest (side,movePiece) board)))]
          where
            takeMsg = if enemy dest then "x" ++ show (snd (board!dest)) else ""
            promoteMsg = if movePiece /= piece then "->" ++ show movePiece else ""

readBoard :: String -> Board
readBoard string = fromList (concat (zipWith (readRow minBound) (lines string) (reverse [minBound..maxBound])))
  where
    readRow :: Column -> String -> Row -> [(Square,(Side,Piece))]
    readRow col (_:side:piece:str) row = (maybe id ((:) . ((,) (col,row))) (readSidePiece side piece)) (nextCol col str row)
    readRow col _ row = []
    nextCol :: Column -> String -> Row -> [(Square,(Side,Piece))]
    nextCol col str row
      | col == maxBound = []
      | otherwise = readRow (succ col) str row
    readSidePiece :: Char -> Char -> Maybe (Side,Piece)
    readSidePiece 'W' piece = fmap ((,) White) (readPiece piece)
    readSidePiece 'B' piece = fmap ((,) Black) (readPiece piece)
    readSidePiece _ _ = Nothing
    readPiece :: Char -> Maybe Piece
    readPiece 'R' = Just R
    readPiece 'N' = Just N
    readPiece 'B' = Just B
    readPiece 'Q' = Just Q
    readPiece 'K' = Just K
    readPiece 'P' = Just P
    readPiece _ = Nothing

showBoard :: Board -> String
showBoard board = unlines [concat [showSquare (c,r) | c <- [minBound..maxBound]] | r <- reverse [minBound..maxBound]]
  where
    showSquare coords = maybe " __" showPiece (Data.Map.lookup coords board)
    showPiece (s,p) = ' ':take 1 (show s) ++ show p

showBoardU :: Board -> [String]
showBoardU board = ((' ':map (head . tail . show) [minBound..maxBound::Column]):[drop 1 (show r) ++ [showSquare (c,r) | c <- [minBound..maxBound]] | r <- reverse [minBound..maxBound]])
  where
    showSquare coords = maybe '_' showPiece (Data.Map.lookup coords board)
    showPiece (Black,R) = '♜'
    showPiece (Black,N) = '♞'
    showPiece (Black,B) = '♝'
    showPiece (Black,Q) = '♛'
    showPiece (Black,K) = '♚'
    showPiece (Black,P) = '♟'
    showPiece (White,R) = '♖'
    showPiece (White,N) = '♘'
    showPiece (White,B) = '♗'
    showPiece (White,Q) = '♕'
    showPiece (White,K) = '♔'
    showPiece (White,P) = '♙'

showMove :: Move -> String
showMove ((sc,sr),(dc,dr),msg) = drop 1 (show sc) ++ drop 1 (show sr) ++ "-" ++ drop 1 (show dc) ++ drop 1 (show dr) ++ ":" ++ msg

matedIn1Moves :: State -> [(Move,[Move])]
matedIn1Moves state = [(m,map fst (mateIn1 s)) | (m,s) <- legalMoves state]

mateIn2Moves :: State -> [(Move,[(Move,[Move])])]
mateIn2Moves = map (fmap matedIn1Moves) . mateIn2

showMoves :: [(Move,[(Move,[Move])])] -> String
showMoves m = unlines (concat (map showM1 m))
  where
    showM1 (m1,m2) = showMove m1 : map showM2 m2
    showM2 (m2,m3) = "  " ++ showMove m2 ++ concatMap ((" "++) . showMove) m3

showMateIn2MovesWithBoard :: (Move,State) -> String
showMateIn2MovesWithBoard (move,state) = unlines (showMove move:concatMap showResponses (legalMoves state))
  where
    showResponses (move2,state2@(_,board)) = ("  " ++ showMove move2 ++ concatMap ((" "++) . showMove . fst) (mateIn1 state2)):map ("    "++) (showBoardU board)

main :: IO ()
main = do
  board <- fmap readBoard getContents
  args <- getArgs
  case args of
    [] -> (putStr . showMoves . mateIn2Moves) (White,board)
    ["matedin1"] -> (mapM_ putStrLn . map (showMove . fst) . filter (null . mateIn1 . snd) .  legalMoves) (Black,board)
    ["board"] -> (mapM_ (putStr . showMateIn2MovesWithBoard) . mateIn2) (White,board)
    _ ->  (putStr . showMoves . mateIn2Moves) (White,board)

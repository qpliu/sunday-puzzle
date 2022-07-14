import Data.Map(Map,delete,fromList,insert,(!))
import qualified Data.Map
import System.Environment(getArgs)

data Row = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 deriving (Bounded,Enum,Eq,Ord,Show)

data Column = Ca | Cb | Cc | Cd | Ce | Cf | Cg | Ch deriving (Bounded,Enum,Eq,Ord,Show)

type Square = (Column,Row)

data Side = White | Black deriving (Eq,Show)

data Piece = R | N | B | Q | K | P deriving (Eq,Show)

type Board = Map Square (Side,Piece)

type State = (Side,Board)

type Move = (Square,Square,Piece,Maybe Piece,Piece) -- (source,destination,piece,taken,possibly-promoted)

mated :: State -> Bool
mated state = (null . legalMoves) state && inCheck state

mateIn1 :: State -> [(Move,State)]
mateIn1 = filter (mated . snd) . legalMoves

matedIn1 :: State -> Bool
matedIn1 = all (not . null . mateIn1 . snd) . legalMoves

mateIn2 :: State -> [(Move,State)]
mateIn2 = filter (matedIn1 . snd) . legalMoves

inCheck :: State -> Bool
inCheck (side,board) = any tookK moves
  where
    enemy | side == White = Black | otherwise = White
    moves = concat [movesFrom enemy (col,row) board | col <- [minBound..maxBound], row <- [minBound..maxBound]]
    tookK ((_,_,_,taken,_),_) = taken == Just K

legalMoves :: State -> [(Move,State)]
legalMoves (side,board) = filter (not . inCheck . snd . snd) (moves side board ++ castling)
  where
    moves side board = concat [movesFrom side (col,row) board | col <- [minBound..maxBound], row <- [minBound..maxBound]]
    enemy | side == White = Black | otherwise = White
    clear square = Nothing == Data.Map.lookup square board
    isAttacked square board = any (isTaken square) (moves enemy board)
      where
        isTaken square (_,(_,board)) = Just enemy == fmap fst (Data.Map.lookup square board)
    inCheck board = or [isK (col,row) && isAttacked (col,row) board | col <- [minBound..maxBound], row <- [minBound..maxBound]]
      where
        isK square = Just (side,K) == Data.Map.lookup square board
    castling :: [(Move,State)]
    castling
      | side == White && Just (White,K) == Data.Map.lookup (Ce,R1) board =
          (if Just (White,R) == Data.Map.lookup (Ca,R1) board
              && clear (Cb,R1) && clear (Cc,R1) && clear (Cd,R1)
              && not (isAttacked (Cc,R1) board)
              && not (isAttacked (Cd,R1) board)
              && not (isAttacked (Ce,R1) board)
             then
                ((((Ce,R1),(Cc,R1),K,Nothing,K),(enemy,delete (Ca,R1) $ delete (Ce,R1) $ insert (Cc,R1) (White,K) $ insert (Cd,R1) (White,R) board)):)
             else
                id)
          (if Just (White,R) == Data.Map.lookup (Ch,R1) board
              && clear (Cg,R1) && clear (Cf,R1)
              && not (isAttacked (Cg,R1) board)
              && not (isAttacked (Cf,R1) board)
              && not (isAttacked (Ce,R1) board)
             then
                [(((Ce,R1),(Cg,R1),K,Nothing,K),(enemy,delete (Ch,R1) $ delete (Ce,R1) $ insert (Cg,R1) (White,K) $ insert (Cf,R1) (White,R) board))]
             else
                [])
      | side == Black && Just (Black,K) == Data.Map.lookup (Cd,R8) board =
          (if Just (Black,R) == Data.Map.lookup (Ch,R8) board
              && clear (Cg,R8) && clear (Cf,R8) && clear (Ce,R8)
              && not (isAttacked (Cf,R1) board)
              && not (isAttacked (Ce,R1) board)
              && not (isAttacked (Cd,R1) board)
             then
                ((((Cd,R8),(Cf,R8),K,Nothing,K),(enemy,delete (Ch,R8) $ delete (Cd,R8) $ insert (Cf,R8) (Black,K) $ insert (Ce,R8) (Black,R) board)):)
             else
                id)
          (if Just (Black,R) == Data.Map.lookup (Ca,R8) board
              && clear (Cb,R8) && clear (Cc,R8)
              && not (isAttacked (Cb,R8) board)
              && not (isAttacked (Cc,R8) board)
              && not (isAttacked (Cd,R8) board)
             then
                [(((Cd,R8),(Cb,R8),K,Nothing,K),(enemy,delete (Ca,R8) $ delete (Cd,R8) $ insert (Cb,R8) (Black,K) $ insert (Cc,R8) (Black,R) board))]
             else
                [])
      | otherwise = []

movesFrom :: Side -> Square -> Board -> [(Move,State)]
movesFrom side square board = maybe [] pieceMoves (Data.Map.lookup square board)
  where
    clear square = maybe True (const False) (Data.Map.lookup square board)
    enemy square = maybe False ((/= side) . fst) (Data.Map.lookup square board)
    friend square = not (clear square) && not (enemy square)
    -- omit en passant
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
        movesR = moveLine square (Just Ca) Nothing pred id ++
                 moveLine square (Just Ch) Nothing succ id ++
                 moveLine square Nothing (Just R1) id pred ++
                 moveLine square Nothing (Just R8) id succ
        movesB = moveLine square (Just Ca) (Just R1) pred pred ++
                 moveLine square (Just Ch) (Just R1) succ pred ++
                 moveLine square (Just Ca) (Just R8) pred succ ++
                 moveLine square (Just Ch) (Just R8) succ succ
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
            (if c /= Ch && r /= R8 && r /= R7 then to (succ c,succ (succ r)) N else []) ++
            (if c /= Ch && c /= Cg && r /= R8 then to (succ (succ c),succ r) N else []) ++
            (if c /= Ch && c /= Cg && r /= R1 then to (succ (succ c),pred r) N else []) ++
            (if c /= Ch && r /= R1 && r /= R2 then to (succ c,pred (pred r)) N else []) ++
            (if c /= Ca && r /= R1 && r /= R2 then to (pred c,pred (pred r)) N else []) ++
            (if c /= Ca && c /= Cb && r /= R1 then to (pred (pred c),pred r) N else []) ++
            (if c /= Ca && c /= Cb && r /= R8 then to (pred (pred c),succ r) N else []) ++
            (if c /= Ca && r /= R8 && r /= R7 then to (pred c,succ (succ r)) N else [])
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
              | otherwise = to dest Q ++ to dest N ++ to dest R ++ to dest B
        to dest movePiece
          | friend dest = []
          | otherwise = [((square,dest,piece,fmap snd (Data.Map.lookup dest board),movePiece),(if side == White then Black else White,delete square (insert dest (side,movePiece) board)))]

readBoard :: String -> Board
readBoard string = fromList (concat (zipWith (readRow minBound) (skipU (lines string)) (reverse [minBound..maxBound])))
  where
    readRow :: Column -> String -> Row -> [(Square,(Side,Piece))]
    readRow Ca (r:str) row | ['R',r] == show row = readURow Ca str row
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
    skipU lines | take 1 lines == [" abcdefgh"] = drop 1 lines
                | otherwise = lines
    readURow col [] row = []
    readURow col ('♜':ps) row = ((col,row),(Black,R)) : nextUCol col ps row
    readURow col ('♞':ps) row = ((col,row),(Black,N)) : nextUCol col ps row
    readURow col ('♝':ps) row = ((col,row),(Black,B)) : nextUCol col ps row
    readURow col ('♛':ps) row = ((col,row),(Black,Q)) : nextUCol col ps row
    readURow col ('♚':ps) row = ((col,row),(Black,K)) : nextUCol col ps row
    readURow col ('♟':ps) row = ((col,row),(Black,P)) : nextUCol col ps row
    readURow col ('♖':ps) row = ((col,row),(White,R)) : nextUCol col ps row
    readURow col ('♘':ps) row = ((col,row),(White,N)) : nextUCol col ps row
    readURow col ('♗':ps) row = ((col,row),(White,B)) : nextUCol col ps row
    readURow col ('♕':ps) row = ((col,row),(White,Q)) : nextUCol col ps row
    readURow col ('♔':ps) row = ((col,row),(White,K)) : nextUCol col ps row
    readURow col ('♙':ps) row = ((col,row),(White,P)) : nextUCol col ps row
    readURow col (_:ps) row = nextUCol col ps row
    nextUCol col ps row
      | col == maxBound = []
      | otherwise = readURow (succ col) ps row

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

showMove :: Int -> Move -> String
showMove justification ((sc,sr),(dc,dr),piece,taken,promotion)
  | (sc,sr,dc,dr,piece) == (Ce,R1,Cc,R1,K) = justify "O-O-O"
  | (sc,sr,dc,dr,piece) == (Ce,R1,Cg,R1,K) = justify "O-O"
  | (sc,sr,dc,dr,piece) == (Cd,R8,Cf,R8,K) = justify "O-O-O"
  | (sc,sr,dc,dr,piece) == (Cd,R8,Cb,R8,K) = justify "O-O"
  | otherwise = (justify . drop 1) (show sc ++ drop 1 (show sr) ++ ":" ++ show piece ++ (if taken /= Nothing then "x" else "")  ++ drop 1 (show dc) ++ drop 1 (show dr) ++ (if piece == promotion then "" else '=':show promotion))
  where
    justify str | justification == 0 = str | otherwise = take justification (str ++ repeat ' ')

matedIn1Moves :: State -> [(Move,[Move])]
matedIn1Moves state = [(m,map fst (mateIn1 s)) | (m,s) <- legalMoves state]

mateIn2Moves :: State -> [(Move,[(Move,[Move])])]
mateIn2Moves = map (fmap matedIn1Moves) . mateIn2

showMoves2 :: (Move,[Move]) -> String
showMoves2 (m,ms) = showMove 10 m ++ " - " ++ concatMap (showMove 11) ms

showMoves3 :: [(Move,[(Move,[Move])])] -> [String]
showMoves3 = concatMap showM1
  where
    showM1 (m1,m2) = showMove 0 m1 : zipWith showM2 m2 [1..]
    showM2 m2 n = " " ++ show n ++ ". " ++ showMoves2 m2

notMatedIn2 :: (Move -> Bool) -> State -> [(Move,[Move])]
notMatedIn2 chooseMoves state = [(move,[m | (m,state3) <- legalMoves state2, (null . mateIn1) state3]) | (move,state2) <- legalMoves state, chooseMoves move]

moveMatches :: String -> Move -> Bool
moveMatches move ((srccol,srcrow),(destcol,destrow),piece,taken,promotion) =
    move == (drop 1 . show) srccol ++ (drop 1 . show) srcrow ++ "-" ++ (drop 1 . show) destcol ++ (drop 1 . show) destrow
    || move == (drop 1 . show) srccol ++ (drop 1 . show) srcrow ++ "-" ++ (drop 1 . show) destcol ++ (drop 1 . show) destrow ++ "=" ++ show promotion
    || move == show piece ++ (drop 1 . show) destcol ++ (drop 1 . show) destrow
    || move == show piece ++ (drop 1 . show) destcol ++ (drop 1 . show) destrow ++ "=" ++ show promotion
    || maybe False ((move ==) . ((show piece ++ "x") ++) . show) taken
    || maybe False ((move ==) . const (show piece ++ "x" ++ (drop 1 . show) destcol ++ (drop 1 . show) destrow)) taken
    || (move `elem` ["ooo","oo","o-o-o","o-o","OOO","OO","O-O-O","O-O"]
        && (srccol,srcrow,destcol,destrow,piece) `elem`
           [(Ce,R1,Cc,R1,K),(Ce,R1,Cg,R1,K),(Cd,R8,Cf,R8,K),(Cd,R8,Cb,R8,K)])

makeMoves :: [String] -> State -> Either String State
makeMoves [] state = return state
makeMoves (move:moves) state =
  case filter (moveMatches move . fst) (legalMoves state) of
    [(_,newState)] -> makeMoves moves newState
    [] -> Left ("invalid move:"++move)
    _ -> Left ("ambiguous move:"++move)

putBoard :: Either String State -> IO ()
putBoard (Left msg) = putStrLn msg
putBoard (Right (_,board)) = (mapM_ putStrLn . showBoardU) board

putMoves :: Either String State -> IO ()
putMoves (Left msg) = putStrLn msg
putMoves (Right state@(side,board)) =
    mapM_ putStrLn ((show side++":"):zipWith showm (legalMoves state) [1..])
  where
    showm (move,state) i
      | mated state = ' ':show i ++ ". " ++ showMove 0 move ++ "#"
      | inCheck state = ' ':show i ++ ". " ++ showMove 0 move ++ "+"
      | otherwise = ' ':show i ++ ". " ++ showMove 0 move

main :: IO ()
main = do
  board <- fmap readBoard getContents
  args <- getArgs
  case args of
    [] -> (mapM_ putStrLn . showMoves3 . mateIn2Moves) (White,board)
    ("board":"black":moves) -> (putBoard . makeMoves moves) (Black,board)
    ("board":"white":moves) -> (putBoard . makeMoves moves) (White,board)
    ("board":moves) -> (putBoard . makeMoves moves) (White,board)
    ("moves":"black":moves) -> (putMoves . makeMoves moves) (Black,board)
    ("moves":"white":moves) -> (putMoves . makeMoves moves) (White,board)
    ("moves":moves) -> (putMoves . makeMoves moves) (White,board)
    ("black":moves) -> (putMoves . makeMoves moves) (Black,board)
    ("white":moves) -> (putMoves . makeMoves moves) (White,board)
    [move] -> (mapM_ (putStrLn . showMoves2) . notMatedIn2 (moveMatches move)) (White,board)
    moves -> (putMoves . makeMoves moves) (White,board)

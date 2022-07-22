import Data.Map(Map,delete,fromList,insert,(!))
import qualified Data.Map
import System.Environment(getArgs)

data Row = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 deriving (Bounded,Enum,Eq,Ord,Show)

data Column = Ca | Cb | Cc | Cd | Ce | Cf | Cg | Ch deriving (Bounded,Enum,Eq,Ord,Show)

type Square = (Column,Row)

data Side = White | Black deriving (Eq,Show)

data Piece = R | N | B | Q | K | P deriving (Eq,Show)

type Board = Map Square (Side,Piece)

type Flags = (Bool,Bool,Bool,Bool,Maybe Square) -- (white queen side castling ok,white king side castling ok,black queen side castling ok,black king side castling ok,en passant allowed)

type State = ((Side,Flags),Board)

type Move = (Square,Square,Piece,Maybe Piece,Piece,Bool) -- (source,destination,piece,taken,possibly-promoted,en-passant)

opp :: Side -> Side
opp White = Black
opp Black = White

mated :: State -> Bool
--mated = (null . legalMoves)
mated state = (null . legalMoves) state && inCheck state

mateIn1 :: State -> [(Move,State)]
mateIn1 = filter (mated . snd) . legalMoves

matedIn1 :: State -> Bool
--matedIn1 = all (not . null . mateIn1 . snd) . legalMoves
matedIn1 state = let states = (map snd . legalMoves) state in all (not . null . mateIn1) states && (not . null) states

mateIn2 :: State -> [(Move,State)]
mateIn2 = filter (matedIn1 . snd) . legalMoves

inCheck :: State -> Bool
inCheck ((side,flags),board) = any tookK moves
  where
    moves = concat [movesFrom flags (opp side) (col,row) board | col <- [minBound..maxBound], row <- [minBound..maxBound]]
    tookK ((_,_,_,taken,_,_),_) = taken == Just K

legalMoves :: State -> [(Move,State)]
legalMoves ((side,flags),board) = filter (not . inCheck . snd . snd) (moves side board ++ castling)
  where
    moves side board = concat [movesFrom flags side (col,row) board | col <- [minBound..maxBound], row <- [minBound..maxBound]]
    enemy = opp side
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
                ((((Ce,R1),(Cc,R1),K,Nothing,K,False),((enemy,flags),delete (Ca,R1) $ delete (Ce,R1) $ insert (Cc,R1) (White,K) $ insert (Cd,R1) (White,R) board)):)
             else
                id)
          (if Just (White,R) == Data.Map.lookup (Ch,R1) board
              && clear (Cg,R1) && clear (Cf,R1)
              && not (isAttacked (Cg,R1) board)
              && not (isAttacked (Cf,R1) board)
              && not (isAttacked (Ce,R1) board)
             then
                [(((Ce,R1),(Cg,R1),K,Nothing,K,False),((enemy,flags),delete (Ch,R1) $ delete (Ce,R1) $ insert (Cg,R1) (White,K) $ insert (Cf,R1) (White,R) board))]
             else
                [])
      | side == Black && Just (Black,K) == Data.Map.lookup (Cd,R8) board =
          (if Just (Black,R) == Data.Map.lookup (Ch,R8) board
              && clear (Cg,R8) && clear (Cf,R8) && clear (Ce,R8)
              && not (isAttacked (Cf,R1) board)
              && not (isAttacked (Ce,R1) board)
              && not (isAttacked (Cd,R1) board)
             then
                ((((Cd,R8),(Cf,R8),K,Nothing,K,False),((enemy,flags),delete (Ch,R8) $ delete (Cd,R8) $ insert (Cf,R8) (Black,K) $ insert (Ce,R8) (Black,R) board)):)
             else
                id)
          (if Just (Black,R) == Data.Map.lookup (Ca,R8) board
              && clear (Cb,R8) && clear (Cc,R8)
              && not (isAttacked (Cb,R8) board)
              && not (isAttacked (Cc,R8) board)
              && not (isAttacked (Cd,R8) board)
             then
                [(((Cd,R8),(Cb,R8),K,Nothing,K,False),((enemy,flags),delete (Ca,R8) $ delete (Cd,R8) $ insert (Cb,R8) (Black,K) $ insert (Cc,R8) (Black,R) board))]
             else
                [])
      | otherwise = []

movesFrom :: Flags -> Side -> Square -> Board -> [(Move,State)]
movesFrom flags@(wooo,woo,booo,boo,enpassant) side square board = maybe [] pieceMoves (Data.Map.lookup square board)
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
            (if r == start && clear (c,forward r) && clear (c,forward (forward r)) then pawnFirstTo (c,forward r) (c,forward (forward r)) else []) ++
            (if clear (c,forward r) then pawnTo (c,forward r) else []) ++
            (if c /= minBound && enemy (pred c,forward r) then pawnTo (pred c,forward r) else []) ++
            (if c /= maxBound && enemy (succ c,forward r) then pawnTo (succ c,forward r) else []) ++
            (if c /= minBound && enpassant == Just (pred c,forward r) then enPassantTo (pred c,r) (pred c,forward r) else []) ++
            (if c /= maxBound && enpassant == Just (succ c,forward r) then enPassantTo (succ c,r) (succ c,forward r) else [])
          where
            pawnTo dest@(_,destrow)
              | destrow /= end = to dest P
              | otherwise = to dest Q ++ to dest N ++ to dest R ++ to dest B
            pawnFirstTo skip dest = map (fmap (enableEnPassant skip)) (to dest P)
            enPassantTo kill dest = map (enPassantKill kill) (to dest P)
            enPassantKill kill ((square,dest,piece,_,movePiece,_),state) = ((square,dest,piece,fmap snd (Data.Map.lookup kill board),movePiece,True),fmap (delete kill) state)
        to dest movePiece
          | friend dest = []
          | otherwise = [((square,dest,piece,fmap snd (Data.Map.lookup dest board),movePiece,False),((opp side,(wooo&&square/=(Ca,R1)&&square/=(Ce,R1),woo&&square/=(Ch,R1)&&square/=(Ce,R1),booo&&square/=(Ch,R8)&&square/=(Cd,R8),boo&&square/=(Ca,R8)&&square/=(Cd,R8),Nothing)),delete square (insert dest (side,movePiece) board)))]

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

showBoard :: Board -> [String]
showBoard board = ((' ':map (head . tail . show) [minBound..maxBound::Column]):[drop 1 (show r) ++ [showSquare (c,r) | c <- [minBound..maxBound]] | r <- reverse [minBound..maxBound]])
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

showMove :: Int -> String -> Move -> String
showMove justification suffix ((sc,sr),(dc,dr),piece,taken,promotion,enPassant)
  | (sc,sr,dc,dr,piece) == (Ce,R1,Cc,R1,K) = justify "O-O-O"
  | (sc,sr,dc,dr,piece) == (Ce,R1,Cg,R1,K) = justify "O-O"
  | (sc,sr,dc,dr,piece) == (Cd,R8,Cf,R8,K) = justify "O-O-O"
  | (sc,sr,dc,dr,piece) == (Cd,R8,Cb,R8,K) = justify "O-O"
  | otherwise = (justify . drop 1) (show sc ++ drop 1 (show sr) ++ ":" ++ show piece ++ (if taken /= Nothing then "x" else "")  ++ drop 1 (show dc) ++ drop 1 (show dr) ++ (maybe "" ((':':) . show) taken) ++ (if piece /= promotion then '=':show promotion else if enPassant then "(ep)" else ""))
  where
    justify str | justification == 0 = str ++ suffix | otherwise = take justification (str ++ suffix ++ repeat ' ')

moveMatches :: String -> Move -> Bool
moveMatches move ((srccol,srcrow),(destcol,destrow),piece,taken,promotion,enPassant) =
    move `elem` list || (piece /= promotion && move `elem` map (++ ('=':show promotion)) list)
    || (promotion /= piece && move == src ++ "-" ++ dest ++ "=" ++ show promotion)
    || (move `elem` ["ooo","o-o-o","OOO","O-O-O"]
        && (srccol,srcrow,destcol,destrow,piece) `elem`
           [(Ce,R1,Cc,R1,K),(Cd,R8,Cf,R8,K)])
    || (move `elem` ["oo","o-o","OO","O-O"]
        && (srccol,srcrow,destcol,destrow,piece) `elem`
           [(Ce,R1,Cg,R1,K),(Cd,R8,Cb,R8,K)])
  where
    src = (drop 1 . show) srccol ++ (drop 1 . show) srcrow
    dest = (drop 1 . show) destcol ++ (drop 1 . show) destrow
    list = [src ++ "-" ++ dest, dest, show piece ++ dest] ++ maybe [] (capturelist . show) taken
    capturelist t = [src ++ "x" ++ dest, "x" ++ dest, show piece ++ "x" ++ dest, show piece ++ "x" ++ t, src ++ "x" ++ t, "x" ++ t]

makeMove :: String -> State -> Either String (Move,State)
makeMove move state =
    case filter (moveMatches move . fst) (legalMoves state) of
      [result] -> Right result
      [] -> Left ("invalid move:"++move)
      _ -> Left ("ambiguous move:"++move)

putMoves :: ([Move],State) -> IO ()
putMoves (moveHistory,state@((side,_),_)) =
    mapM_ putStrLn (header:zipWith showm (legalMoves state) [1..])
  where
    header = (concatMap (((show (opp side) ++ ": ") ++) . (++ ", ") . showMove 0 "") . take 1) moveHistory ++ show side ++ ":"
    showm (move,state2) i
      | mated state2 = ' ':showEnum i ++ showMove 0 "#" move
      | otherwise = ' ':showEnum i ++ showMove 14 (if inCheck state2 then "+" else "") move ++ " - " ++ concatMap (showMove 14 "#" . fst) (mateIn1 state2)

showEnum :: Int -> String
showEnum i = take 5 (show i ++ ".   ")

modify :: String -> Board -> Board
modify move = case move of
    ('x':m) -> dosquare m delete
    ('W':m) -> dopiece White m
    ('B':m) -> dopiece Black m
    _ -> id
  where
    dopiece :: Side -> String -> Board -> Board
    dopiece side ('R':m) = dosquare m (flip insert (side,R))
    dopiece side ('N':m) = dosquare m (flip insert (side,N))
    dopiece side ('B':m) = dosquare m (flip insert (side,B))
    dopiece side ('Q':m) = dosquare m (flip insert (side,Q))
    dopiece side ('K':m) = dosquare m (flip insert (side,K))
    dopiece side ('P':m) = dosquare m (flip insert (side,P))
    dopiece _ _ = id
    dosquare :: String -> (Square -> Board -> Board) -> Board -> Board
    dosquare m domod = maybe id domod (parseSquare m)

enableEnPassant :: Square -> State -> State
enableEnPassant square ((side,(wooo,woo,booo,boo,_)),board) = ((side,(wooo,woo,booo,boo,Just square)),board)

parseSquare :: String -> Maybe Square
parseSquare [row,col] = last (Nothing:[Just (r,c) | r <- [minBound..maxBound], row == last (show r), c <- [minBound..maxBound], col == last (show c)])
parseSquare _ = Nothing

processArgs :: [String] -> [Move] -> State -> IO ()
processArgs ("white":args) moves ((side,flags),board) = processArgs args moves ((White,flags),board)
processArgs ("black":args) moves ((side,flags),board) = processArgs args moves ((Black,flags),board)
processArgs ("solve":_) _ state = (mapM_ putMoves . map (\ (a,b) -> ([a],b)) . mateIn2) state
processArgs ("board":_) _ (_,board) = (mapM_ putStrLn . showBoard) board
processArgs (('e':'p':':':square):args) moves state = processArgs args moves ((maybe id enableEnPassant (parseSquare square)) state)
-- add option to make castling unavailable
processArgs (('-':mod):args) moves ((side,flags),board) = processArgs args moves ((side,flags),modify mod board)
processArgs (arg:args) moves state = either putStrLn (\ (move,state) -> processArgs args (move:moves) state) (makeMove arg state)
processArgs [] moves state = putMoves (moves,state)

possibleEnPassant :: Side -> Board -> Maybe Square
possibleEnPassant side board | side == White = check R5 R6
                             | otherwise = check R4 R3
  where check pawnrow skiprow | length possibles == 1 = (Just . head) possibles
                              | otherwise = Nothing
          where possibles = [(col,skiprow) | col <- [Ca .. Ch], Nothing == Data.Map.lookup (col,skiprow) board, Just (opp side,P) == Data.Map.lookup (col,pawnrow) board]

main :: IO ()
main = do
  board <- fmap readBoard getContents
  args <- getArgs
  processArgs (if null args then ["solve"] else args) [] ((White,(True,True,True,True,possibleEnPassant White board)),board)

import Control.Monad(foldM)
import Data.Char(ord,toLower)
import Data.Foldable(maximumBy)
import Data.List(intercalate)
import Data.Map(Map,alter,delete,empty,fromList,insert,member,toList,(!))
import qualified Data.Map
import System.Environment(getArgs)
import Text.Read(readMaybe)

data Row = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 deriving (Bounded,Enum,Eq,Ord,Show)

data Column = Ca | Cb | Cc | Cd | Ce | Cf | Cg | Ch deriving (Bounded,Enum,Eq,Ord,Show)

type Square = (Column,Row)

data Side = White | Black deriving (Eq,Show)

data Piece = R | N | B | Q | K | P deriving (Eq,Show)

type Board = Map Square (Side,Piece)

type Flags = (Bool,Bool,Bool,Bool,Maybe Square,Int,Int) -- (white queen side castling ok,white king side castling ok,black queen side castling ok,black king side castling ok,en passant allowed,stale mate count,move number)

type State = ((Side,Flags),Board)

type Move = (Square,Square,Piece,Maybe Piece,Piece,Bool) -- (source,destination,piece,taken,possibly-promoted,en-passant)

opp :: Side -> Side
opp White = Black
opp Black = White

mated :: State -> Bool
--mated = null . legalMoves
mated = flip all [null . legalMoves, inCheck] . flip ($)

mateIn1 :: State -> [(Move,State)]
mateIn1 = filter (mated . snd) . legalMoves

matedIn1 :: State -> Bool
--matedIn1 = all (not . null . mateIn1 . snd) . legalMoves
matedIn1 = flip all [all (not . null . mateIn1 . snd), not . null] . flip ($) . legalMoves

mateIn2 :: State -> [(Move,State)]
mateIn2 = filter (matedIn1 . snd) . legalMoves

inCheck :: State -> Bool
inCheck ((side,flags),board) = any tookK moves
  where
    moves = concat [movesFrom flags (opp side) (col,row) board | col <- [minBound..maxBound], row <- [minBound..maxBound]]
    tookK ((_,_,_,taken,_,_),_) = taken == Just K

legalMoves :: State -> [(Move,State)]
legalMoves ((side,flags@(wooo,woo,booo,boo,_,stalemateCount,moveNumber)),board) = filter (not . inCheck . snd . snd) (moves side board ++ castling)
  where
    moves side board = concat [movesFrom flags side (col,row) board | row <- [R8,R7 .. R1], col <- [Ca .. Ch]]
    newMoveNumber = if side == White then moveNumber else moveNumber + 1
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
          (if wooo && Just (White,R) == Data.Map.lookup (Ca,R1) board
              && clear (Cb,R1) && clear (Cc,R1) && clear (Cd,R1)
              && not (isAttacked (Cc,R1) board)
              && not (isAttacked (Cd,R1) board)
              && not (isAttacked (Ce,R1) board)
             then
                ((((Ce,R1),(Cc,R1),K,Nothing,K,False),((enemy,(False,False,booo,boo,Nothing,0,newMoveNumber)),delete (Ca,R1) $ delete (Ce,R1) $ insert (Cc,R1) (White,K) $ insert (Cd,R1) (White,R) board)):)
             else
                id)
          (if woo && Just (White,R) == Data.Map.lookup (Ch,R1) board
              && clear (Cg,R1) && clear (Cf,R1)
              && not (isAttacked (Cg,R1) board)
              && not (isAttacked (Cf,R1) board)
              && not (isAttacked (Ce,R1) board)
             then
                [(((Ce,R1),(Cg,R1),K,Nothing,K,False),((enemy,(False,False,booo,boo,Nothing,0,newMoveNumber)),delete (Ch,R1) $ delete (Ce,R1) $ insert (Cg,R1) (White,K) $ insert (Cf,R1) (White,R) board))]
             else
                [])
      | side == Black && Just (Black,K) == Data.Map.lookup (Cd,R8) board =
          (if booo && Just (Black,R) == Data.Map.lookup (Ch,R8) board
              && clear (Cg,R8) && clear (Cf,R8) && clear (Ce,R8)
              && not (isAttacked (Cf,R1) board)
              && not (isAttacked (Ce,R1) board)
              && not (isAttacked (Cd,R1) board)
             then
                ((((Cd,R8),(Cf,R8),K,Nothing,K,False),((enemy,(wooo,woo,False,False,Nothing,0,newMoveNumber)),delete (Ch,R8) $ delete (Cd,R8) $ insert (Cf,R8) (Black,K) $ insert (Ce,R8) (Black,R) board)):)
             else
                id)
          (if boo && Just (Black,R) == Data.Map.lookup (Ca,R8) board
              && clear (Cb,R8) && clear (Cc,R8)
              && not (isAttacked (Cb,R8) board)
              && not (isAttacked (Cc,R8) board)
              && not (isAttacked (Cd,R8) board)
             then
                [(((Cd,R8),(Cb,R8),K,Nothing,K,False),((enemy,(wooo,woo,False,False,Nothing,0,newMoveNumber)),delete (Ca,R8) $ delete (Cd,R8) $ insert (Cb,R8) (Black,K) $ insert (Cc,R8) (Black,R) board))]
             else
                [])
      | otherwise = []

movesFrom :: Flags -> Side -> Square -> Board -> [(Move,State)]
movesFrom flags@(wooo,woo,booo,boo,enpassant,stalemateCount,moveNumber) side square board = maybe [] pieceMoves (Data.Map.lookup square board)
  where
    clear square = maybe True (const False) (Data.Map.lookup square board)
    enemy square = maybe False ((/= side) . fst) (Data.Map.lookup square board)
    friend square = not (clear square) && not (enemy square)
    newMoveNumber = if side == White then moveNumber else moveNumber+1
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
          | otherwise = [((square,dest,piece,fmap snd (Data.Map.lookup dest board),movePiece,False),((opp side,(wooo&&square/=(Ca,R1)&&square/=(Ce,R1),woo&&square/=(Ch,R1)&&square/=(Ce,R1),booo&&square/=(Ch,R8)&&square/=(Cd,R8),boo&&square/=(Ca,R8)&&square/=(Cd,R8),Nothing,if movePiece == P || Data.Map.lookup dest board /= Nothing then 0 else stalemateCount+1,newMoveNumber)),delete square (insert dest (side,movePiece) board)))]

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

showSquare :: Square -> String
showSquare (col,row) = tail (show col) ++ tail (show row)

fen :: State -> String
fen ((side,(wooo,woo,booo,boo,ep,stalemateCount,moveNumber)),board) =
    intercalate "/" (map fenRow [R8,R7 .. R1])
      ++ (if side == White then " w " else " b ")
      ++ (if wk then "K" else "")
      ++ (if wq then "Q" else "")
      ++ (if bk then "k" else "")
      ++ (if bq then "q" else "")
      ++ (if not (wk || wq || bk || bq) then "-" else "")
      ++ " " ++ maybe "-" showSquare ep
      ++ " " ++ show stalemateCount ++ " " ++ show moveNumber
  where
    wk = woo && Just (White,K) == Data.Map.lookup (Ce,R1) board && Just (White,R) == Data.Map.lookup (Ch,R1) board
    wq = wooo && Just (White,K) == Data.Map.lookup (Ce,R1) board && Just (White,R) == Data.Map.lookup (Ca,R1) board
    bk = boo && Just (Black,K) == Data.Map.lookup (Cd,R8) board && Just (Black,R) == Data.Map.lookup (Ca,R8) board
    bq = booo && Just (Black,K) == Data.Map.lookup (Cd,R8) board && Just (Black,R) == Data.Map.lookup (Ch,R8) board
    fenRow row = (if blanks > 0 then show blanks else "") ++ desc
      where
        (blanks,desc) = foldr buildDesc (0,"") [Ca .. Ch]
        buildDesc col (blanks,desc) = maybe (blanks+1,desc) (addDesc blanks desc) (Data.Map.lookup (col,row) board)
        addDesc blanks desc (side,piece) =
          (0,(if side == White then show else (map toLower . show)) piece
              ++ (if blanks > 0 then show blanks else "") ++ desc)

readFen :: String -> Maybe State
readFen str = do
    (board,str) <- parseBoard str
    (side,str) <- parseSide str
    flags <- parseFlags str
    return ((side,flags),board)
  where
    parseBoard str = do
        (squares,str) <- foldM parseRow ([],'/':str) [R8,R7 .. R1]
        Just (fromList squares,str)
    parseRow (squares,'/':str) row = do
        (squares,str,blanks) <- foldM (parseCol row) (squares,str,0) [Ca .. Ch]
        if blanks /= 0 then Nothing else return (squares,str)
    parseRow _ _ = Nothing
    parseCol row (squares,str,blanks) col
      | blanks > 0 = Just (squares,str,blanks-1)
      | null str = Nothing
      | head str `elem` "12345678" = Just (squares,tail str,ord (head str) - ord '1')
      | head str == 'P' = Just (((col,row),(White,P)):squares,tail str,0)
      | head str == 'R' = Just (((col,row),(White,R)):squares,tail str,0)
      | head str == 'N' = Just (((col,row),(White,N)):squares,tail str,0)
      | head str == 'B' = Just (((col,row),(White,B)):squares,tail str,0)
      | head str == 'Q' = Just (((col,row),(White,Q)):squares,tail str,0)
      | head str == 'K' = Just (((col,row),(White,K)):squares,tail str,0)
      | head str == 'p' = Just (((col,row),(Black,P)):squares,tail str,0)
      | head str == 'r' = Just (((col,row),(Black,R)):squares,tail str,0)
      | head str == 'n' = Just (((col,row),(Black,N)):squares,tail str,0)
      | head str == 'b' = Just (((col,row),(Black,B)):squares,tail str,0)
      | head str == 'q' = Just (((col,row),(Black,Q)):squares,tail str,0)
      | head str == 'k' = Just (((col,row),(Black,K)):squares,tail str,0)
      | otherwise = Nothing
    parseSide (' ':'b':' ':str) = Just (Black,str)
    parseSide (' ':'w':' ':str) = Just (White,str)
    parseSide _ = Nothing
    parseFlags ('-':' ':str) = parseEP False False False False str
    parseFlags str = parseCastle False False False False str
    parseCastle wk wq bk bq (' ':str) = parseEP wk wq bk bq str
    parseCastle wk wq bk bq ('K':str) = parseCastle True wq bk bq str
    parseCastle wk wq bk bq ('Q':str) = parseCastle wk True bk bq str
    parseCastle wk wq bk bq ('k':str) = parseCastle wk wq True bq str
    parseCastle wk wq bk bq ('q':str) = parseCastle wk wq bk True str
    parseCastle _ _ _ _ _ = Nothing
    parseEP wk wq bk bq ('-':' ':str) = parseNumbers wk wq bk bq Nothing str
    parseEP wk wq bk bq str@(_:_:' ':_) = do
      square <- parseSquare (take 2 str)
      parseNumbers wk wq bk bq (Just square) (drop 3 str)
    parseEP _ _ _ _ _ = Nothing
    parseNumbers wk wq bk bq ep str = do
      wds <- return (words str)
      stalemateCount <- if length wds > 1 then readMaybe (wds!!0) else Nothing
      moveNumber <- if length wds > 1 then readMaybe (wds!!1) else Nothing
      return (wq,wk,bq,bk,ep,stalemateCount,moveNumber)

moveNotations :: (Move,a) -> [String]
moveNotations (((sc,sr),(dc,dr),piece,taken,promotion,enPassant),_)
  | (sc,sr,dc,dr,piece) == (Ce,R1,Cc,R1,K) = ["O-O-O","0-0-0","OOO","000","ooo"]
  | (sc,sr,dc,dr,piece) == (Ce,R1,Cg,R1,K) = ["O-O","0-0","OO","00","oo"]
  | (sc,sr,dc,dr,piece) == (Cd,R8,Cf,R8,K) = ["O-O-O","0-0-0","OOO","000","ooo"]
  | (sc,sr,dc,dr,piece) == (Cd,R8,Cb,R8,K) = ["O-O","0-0","OO","00","oo"]
  | promotion /= piece = map (++ ("=" ++ show promotion)) (pawnNotations taken ++ notations taken)
  | enPassant = map (++ "e.p.") (pawnNotations taken ++ notations taken) ++ pawnNotations taken ++ notations taken
  | piece == P = pawnNotations taken ++ notations taken
  | otherwise = notations taken
  where
    src = showSquare (sc,sr)
    dest = showSquare (dc,dr)
    srcFile = (tail . show) sc
    srcRank = (tail . show) sr
    destFile = (tail . show) dc
    destRank = (tail . show) dr
    pawnNotations Nothing = [dest]
    pawnNotations (Just taken) = [srcFile++"x"++dest]
    notations Nothing = [
        show piece ++ dest,
        show piece ++ srcFile ++ dest,
        show piece ++ srcRank ++ dest,
        show piece ++ src ++ dest,
        src ++ "-" ++ dest]
    notations (Just taken) = [
        show piece ++ "x" ++ dest,
        show piece ++ srcFile ++ "x" ++ dest,
        show piece ++ srcRank ++ "x" ++ dest,
        show piece ++ src ++ "x" ++ dest,
        show piece ++ "x" ++ show taken,
        src ++ "x" ++ dest] ++ notations Nothing

notateMoves :: [(Move,State)] -> [(String,([String],(Move,State)))]
notateMoves moves = map addDisambiguated (zip notated moves)
  where
    notated = map moveNotations moves
    counts = foldr (alter (Just . maybe 1 (+1))) empty (concat notated)
    addDisambiguated item@(notations,(_,state)) =
      (head (dropWhile ((/= 1) . (counts!)) notations) ++ checks state,item)
    checks state
      | (not . inCheck) state = ""
      | (not . null . legalMoves) state = "+"
      | otherwise = "#"

makeMove :: String -> State -> Either String (String,State)
makeMove move state@((side,_),_) =
    case (filter (elem move . fst . snd) . notateMoves . legalMoves) state of
      [(m,(_,(_,state)))] -> Right (m,state)
      [] -> Left ("invalid move: "++move)
      m -> Left ("ambiguous move "++move++": "++intercalate " " (map fst m))

putMoves :: [String] -> State -> IO ()
putMoves lastMoves state@((side,(_,_,_,_,_,_,moveNumber)),_) =
    (mapM_ putItem . zip [1..] . notateMoves . legalMoves) state
  where
    (prefix,middle)
      | side == White && moveNumber == 1 = (" 1. "," ")
      | side == White = (" "++show (moveNumber-1)++". "++(if length lastMoves > 1 then head (drop 1 lastMoves) else "-")++" "++(if length lastMoves > 0 then head lastMoves else "-")++" "++show moveNumber++". "," ")
      | otherwise = (" "++show moveNumber++". "++(if length lastMoves > 0 then head lastMoves else "-")++" "," "++show (moveNumber+1)++". ")   
    putItem (i,(notation,(_,(_,state2)))) = do
        putStr (" " ++ showEnum i ++ prefix ++ justify notation ++ middle)
        (putStrLn . intercalate "/" . map fst . filter (mated . snd . snd . snd) . notateMoves . legalMoves) state2
    justify str = take ((maximum . map (length . fst) . notateMoves . legalMoves) state) (str ++ repeat ' ')
  
putSolves :: State -> IO ()
putSolves state@((side,(_,_,_,_,_,_,moveNumber)),_) = (mapM_ putSolve . filter (matedIn1 . snd . snd . snd) . notateMoves . legalMoves) state
  where
    putSolve (move1,(_,(_,state2))) = do
        mapM_ (putStrLn . showItem ((maximum . map (getWidth 4) . toList) mergedMove3s) move1) (toList mergedMove3s)
      where
        move2s = (map collectMove3s . notateMoves . legalMoves) state2
        collectMove3s (move2,(_,(_,state3))) = (move2,(map fst . filter (mated . snd . snd . snd) . notateMoves . legalMoves) state3)
        move3Counts = foldr (alter (Just . maybe 1 (+1))) empty (concatMap snd move2s)
        pickMove3 = maximumBy (\ a b -> compare (move3Counts!a) (move3Counts!b))
        move3s = foldl (\ m (m2,m3) -> alter (Just . maybe ([m2]) (m2:)) m3 m) empty (map (fmap pickMove3) move2s)
        mergedMove3s = -- merge captures and non-captures
            (fromList . map doMerge . filter (not . isMergedNoncapture . fst) . toList) move3s
          where
            isMergedNoncapture move@(p1:p2:dest) = (p1:'x':p2:dest) `member` move3s || (p1:p2:'x':dest) `member` move3s
            isMergedNoncapture _ = False
            doMerge (m2@(p:'x':dest),m3list) | (p:dest) `member` move3s =
                (p:'(':'x':')':dest,m3list ++ move3s!(p:dest))
            doMerge (m2@(p1:p2:'x':dest),m3list) | (p1:p2:dest) `member` move3s =
                (p1:p2:'(':'x':')':dest,m3list ++ move3s!(p1:p2:dest))
            doMerge mlist = mlist
    (sep1,sep2,sep3) | side == White = (" "++show moveNumber++". "," "," "++show (moveNumber+1)++". ")
                     | otherwise = (" "++show moveNumber++". - "," "++show (moveNumber+1)++". "," ")
    getWidth n (_,move2s)
      | length move2s <= n = length (intercalate "/" move2s)
      | otherwise = length ("(" ++ show (length move2s) ++ " moves)")
    showItem width move1 (move3,move2s) = sep1 ++ move1 ++ sep2 ++ justify width (intercalate "/" move2s) ("(" ++ show (length move2s) ++ " moves)") ++ sep3 ++ move3
    justify n str str2
      | length str <= n = str ++ (take (n - length str) . repeat) ' '
      | otherwise = str2 ++ (take (n - length str2) . repeat) ' '

putSolution :: State -> IO ()
putSolution state = (mapM_ (putStrLn . fst) . filter (matedIn1 . snd . snd . snd) . notateMoves . legalMoves) state

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
enableEnPassant square ((side,(wooo,woo,booo,boo,_,stalemateCount,moveNumber)),board) = ((side,(wooo,woo,booo,boo,Just square,stalemateCount,moveNumber)),board)

parseSquare :: String -> Maybe Square
parseSquare [row,col] = last (Nothing:[Just (r,c) | r <- [minBound..maxBound], row == last (show r), c <- [minBound..maxBound], col == last (show c)])
parseSquare _ = Nothing

processArgs :: [String] -> [String] -> State -> IO ()
processArgs ("white":args) lastMoves ((side,flags),board) = processArgs args lastMoves ((White,flags),board)
processArgs ("black":args) lastMoves ((side,flags),board) = processArgs args lastMoves ((Black,flags),board)
processArgs ("moves":_) lastMoves state = putMoves lastMoves state
processArgs ("solve":_) _ state = putSolves state
processArgs ("solution":_) _ state = putSolution state
processArgs ("board":_) _ (_,board) = (mapM_ putStrLn . showBoard) board
processArgs ("fen":_) _ state = (putStrLn . fen) state
processArgs (('e':'p':':':square):args) lastMove state = processArgs args lastMove ((maybe id enableEnPassant (parseSquare square)) state)
-- add option to make castling unavailable
processArgs (('-':mod):args) lastMoves ((side,flags),board) = processArgs args lastMoves ((side,flags),modify mod board)
processArgs (arg:args) lastMoves state = either putStrLn (\ (lastMove,state) -> processArgs args (lastMove:lastMoves) state) (makeMove arg state)
processArgs [] [] state = putSolves state
processArgs [] lastMoves state = putMoves lastMoves state

readState :: String -> State
readState str = maybe ((makeState . readBoard) str) id (readFen str)
  where
    makeState board = ((White,(True,True,True,True,possibleEnPassant White board,0,1)),board)
    possibleEnPassant side board | side == White = check R5 R6 R7
                                 | otherwise = check R4 R3 R2
      where check pawnrow skiprow initialrow | length possibles == 1 = (Just . head) possibles
                                             | otherwise = Nothing
              where possibles = [(col,skiprow) | col <- [Ca .. Ch], Nothing == Data.Map.lookup (col,skiprow) board, Nothing == Data.Map.lookup (col,initialrow) board, Just (opp side,P) == Data.Map.lookup (col,pawnrow) board]

main :: IO ()
main = do
  state <- fmap readState getContents
  args <- getArgs
  processArgs (if null args then ["solve"] else args) [] state

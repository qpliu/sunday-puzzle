module AOC201813 where

import Data.List(sort)
import Data.Array(Array,assocs,(//),(!))
import Data.Map(Map,delete,empty,fromList,insert,member)
import qualified Data.Map

import AOC

aoc = AOC {
    day="../../2018/input/13",
    aocTests=[
        AOCTest {
            testData=unlines [
                "/->-\\        ",
                "|   |  /----\\",
                "| /-+--+-\\  |",
                "| | |  | v  |",
                "\\-+-/  \\-+--/",
                "  \\------/   "
                ],
            testResult=Just "(7,3)",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "/>-<\\  ",
                "|   |  ",
                "| /<+-\\",
                "| | | v",
                "\\>+</ |",
                "  |   ^",
                "  \\<->/"
                ],
            testResult=Nothing,
            testResult2=Just "(6,4)"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

type XY = (Int,Int)
type CartID = (Int,Int)
type Cart = (Int,Int,Char,Int,CartID) -- y,x,dir,next turn,cartId
type State = ([Cart],Map XY CartID)

parse input = (grid // removeCarts,(sort carts,fromList cartIds))
  where
    grid = parse2da input
    carts = concatMap makeCart $ assocs grid
    cartIds = map makeCartId carts
    removeCarts = map makeRemoveCart carts
    
    makeCart ((x,y),ch)
      | elem ch "<>^v" = [(y,x,ch,-1,(x,y))]
      | otherwise = []
    makeCartId (_,_,_,_,cartId) = (cartId,cartId)
    makeRemoveCart (_,_,dir,_,xy)
      | elem dir "<>" = (xy,'-')
      | elem dir "v^" = (xy,'|')

tick :: Array XY Char -> State -> ([XY],State)
tick grid state = step [] [] state
  where
    step crashes moved ([],cartLocs) =
        (crashes,(sort $ filter (survivor cartLocs) moved,cartLocs))
    step crashes moved (((y,x,dir,turn,cartID):carts),cartLocs)
      | Just cartID /= Data.Map.lookup (x,y) cartLocs =
          step crashes moved (carts,cartLocs)
      | member nextXY cartLocs =
          step (nextXY:crashes) moved
               (carts,delete (x,y) $ delete nextXY cartLocs)
      | otherwise =
          step crashes ((nextY,nextX,nextDir,nextTurn,cartID):moved)
               (carts,delete (x,y) $ insert nextXY cartID cartLocs)
      where
        nextXY@(nextX,nextY)
          | dir == '^' = (x,y-1)
          | dir == '>' = (x+1,y)
          | dir == 'v' = (x,y+1)
          | dir == '<' = (x-1,y)
        track = grid!nextXY
        (nextDir,nextTurn)
          | track == '|'  && dir == '^' = ('^',turn)
          | track == '|'  && dir == 'v' = ('v',turn)
          | track == '-'  && dir == '<' = ('<',turn)
          | track == '-'  && dir == '>' = ('>',turn)
          | track == '/'  && dir == '^' = ('>',turn)
          | track == '/'  && dir == '>' = ('^',turn)
          | track == '/'  && dir == 'v' = ('<',turn)
          | track == '/'  && dir == '<' = ('v',turn)
          | track == '\\' && dir == '^' = ('<',turn)
          | track == '\\' && dir == '>' = ('v',turn)
          | track == '\\' && dir == 'v' = ('>',turn)
          | track == '\\' && dir == '<' = ('^',turn)
          | track == '+'  && turn == 0 = (dir,1)
          | track == '+'  && turn == -1 && dir == '^' = ('<',0)
          | track == '+'  && turn == -1 && dir == '>' = ('^',0)
          | track == '+'  && turn == -1 && dir == 'v' = ('>',0)
          | track == '+'  && turn == -1 && dir == '<' = ('v',0)
          | track == '+'  && turn ==  1 && dir == '^' = ('>',-1)
          | track == '+'  && turn ==  1 && dir == '>' = ('v',-1)
          | track == '+'  && turn ==  1 && dir == 'v' = ('<',-1)
          | track == '+'  && turn ==  1 && dir == '<' = ('^',-1)
          | otherwise = error (show (nextXY,grid!nextXY,turn,dir))

    survivor cartLocs (y,x,_,_,cartID) =
        Just cartID == Data.Map.lookup (x,y) cartLocs

result :: (Array XY Char,State) -> XY
result (grid,state) = loop ([],state)
  where
    loop (crashes,state)
      | null crashes = loop $ tick grid state
      | otherwise = last crashes

result2 :: (Array XY Char,State) -> XY
result2 (grid,state) = loop state
  where
    loop ([(y,x,_,_,_)],_) = (x,y)
    loop state = loop $ snd $ tick grid state


module Main (main) where

import Control.Monad (when)
import Data.Map (Map)
--import Data.Set (Set)
import Prelude hiding (Left,Right)
import System.Environment (getArgs)
import qualified Data.Map.Strict as Map
--import qualified Data.Set as Set

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ "rubik: " <> show args

  -- 7...
  --let scambleSequence = [R,U,L,R,B,L',U'] -- (track expanded: 2120 steps, 1.7s)

  -- 8..
  --let scambleSequence = [R,U,L,R,B,L',U',F] -- (track expanded 20785 steps, 3m25)

  -- dont track expanded...
  --let scambleSequence = [R,U,L,R,B,L',U',F] -- 26025 steps, (see 100: 4.7s) (see 1000; 1.8s)

  -- 9..
  let scambleSequence = [R,U,L,R,B,L',U',F,D] -- 453730 steps, 1m40
  -- only view every 100,000 steps -> 26s
  -- ok, so counting the frontier size is expensive.
  -- but nice to know, so instead lets track it exlicitly
  -- -> 18s
  -- -> 17s (remove expanded/frontierSize entirely)
  -- -> 15s (some strictness annotations)

  -- 10.. nope!! blows the fans!
  --let scambleSequence = [R,U,L,R,B,L',U',F,D,F]

  let scrambledState = foldl move solvedState scambleSequence
  putStrLn $ unlines (_prettyState scrambledState)
  print (distance solvedState scrambledState)

  let links = _allMoves

  (i,sol) <- viewSearch (mkGraph links) scrambledState

  putStrLn $ concat["Found solution: ", show sol, ", in ", show i, " steps."]
  let (Elem Path{inReverse=steps} _) = sol
  let final = foldl move scrambledState (reverse steps)
  print (final == solvedState)
  putStrLn $ unlines (_prettyState final)


-- using path-length + distance-to-target heuristic
computeScore :: Elem -> Score
computeScore (Elem path state) = do
  let g = pathLength path
  let h = distance solvedState state
  Score (g + h)

viewSearch :: Graph -> State -> IO (Int,Elem)
viewSearch graph scrambled = loop 0 (mkInitSS scrambled)
  where
    loop :: Int -> SS -> IO (Int,Elem)
    loop i ss = do
      when (i `mod` 10000 == 0) $ see i ss;
      case searchStep graph ss of
        Fail -> error "Search failed!"
        Success elem -> return (i,elem)
        Continue ss -> loop (i+1) ss

    see :: Int -> SS -> IO ()
    see i SS {focus, frontier=_f
             --, frontierSize=_fz
             } = do
      print (i,
             --Set.size expanded,
             focus,
             _frontierKeys _f
             --_fz -- frontierSize -- always (b-1) * i, where b is the #links
            )

----------------------------------------------------------------------
-- Path

data Path = Path { inReverse :: [Move] }

instance Show Path where
  show Path {inReverse=xs} =
    "(" <> show (length xs) <> ")" <> concat (map show (reverse xs))

emptyPath :: Path
emptyPath = Path { inReverse = [] }

postPend :: Path -> Move -> Path
postPend Path{inReverse} move = Path {inReverse = move : inReverse}

pathLength :: Path -> Int
pathLength Path{inReverse} = length inReverse

----------------------------------------------------------------------
-- Elem, Score

data Elem = Elem !Path !State

instance Show Elem where
  show (Elem path state) =
    show path <> "~" <> show (distance solvedState state)

newtype Score = Score Int deriving (Eq,Ord)

instance Show Score where
  show (Score n) = "*" <> show n

----------------------------------------------------------------------
-- Graph

data Graph = Graph { links :: State -> [(Move,State)]
                   , target :: State -> Bool }

mkGraph :: [Move] -> Graph
mkGraph moves = Graph { links = \s -> map (\m -> (m, move s m)) moves
                      , target = \s -> s == solvedState
                      }
----------------------------------------------------------------------
-- Frontier

newtype Frontier = Frontier (Map Score [Elem]) deriving (Show)

_frontierKeys :: Frontier -> [Score]
_frontierKeys (Frontier m) = Map.keys m

emptyFrontier :: Frontier
emptyFrontier = Frontier Map.empty

extendFrontier :: Frontier -> Elem -> Frontier
extendFrontier (Frontier m) e = do
  let key = computeScore e
  Frontier (Map.alter (\case Nothing -> Just [e]
                             Just es -> Just (e:es)) key m)

pickFrontier :: Frontier -> Maybe (Elem,Frontier)
pickFrontier (Frontier m) =
  if Map.null m then Nothing else do
    let ((k,es),m') = Map.deleteFindMin m
    case es of
      [] -> pickFrontier (Frontier m')
      e:es' -> Just (e, Frontier (Map.insert k es' m'))

----------------------------------------------------------------------
-- Search State, Search

data SS = SS { focus :: !Elem
             --, expanded :: Set State
             , frontier :: !Frontier
             -- , frontierSize :: Int
             }

data SSK = Fail | Success Elem | Continue SS

mkInitSS :: State -> SS
mkInitSS scrambled = SS { focus = Elem emptyPath scrambled
                        --, expanded = Set.empty
                        , frontier = emptyFrontier
                        -- , frontierSize = 0
                        }

searchStep :: Graph -> SS -> SSK
searchStep Graph{links,target} SS{focus = Elem path state,
                                  --expanded = ex0,
                                  frontier = fr0
                                  --frontierSize -- = fz0
                                 } =

  if target state then Success (Elem path state) else do
    case pickFrontier fr1 of
      Nothing -> Fail
      Just (focus,frontier) -> do
        --let frontierSize = length newElems + fz0 - 1
        Continue $ SS { focus
                      -- , expanded
                      , frontier
                      -- , frontierSize
                      }
  where
    --expanded = ex0 --Set.insert state ex0 -- DONT TRACK EXPANDED
    --expanded = Set.insert state ex0 -- DO TRACK EXPANDED

    fr1 = foldl extendFrontier fr0 newElems
    newElems = [ Elem (postPend path m) n | (m,n) <- links state] --, n `notElem` expanded]

{-
    pickUnexpanded :: Frontier -> Maybe (Elem,Frontier)
    pickUnexpanded frontier =
      case pickFrontier frontier of
        Nothing -> Nothing
        Just res -> Just res
        --Just res@(Elem _ _state, _frontier') ->
          --if _state `notElem` expanded then Just res else
            --pickUnexpanded _frontier'
-}

----------------------------------------------------------------------
-- moves

data Move = F  | B  | U  | D  | L  | R
          | F' | B' | U' | D' | L' | R' deriving (Show,Bounded,Enum)

_allMoves :: [Move]
_allMoves = [minBound..maxBound]

_clocks :: [Move]
_clocks = [F,B,U,D,L,R]

move :: State -> Move -> State
move = flip $ \case
  F -> clock Front
  B -> clock Back
  U -> clock Up
  D -> clock Down
  L -> clock Left
  R -> clock Right
  F'-> anti Front
  B'-> anti Back
  U'-> anti Up
  D'-> anti Down
  L'-> anti Left
  R'-> anti Right

anti :: Face -> State -> State
anti face = clock face . clock face . clock face


----------------------------------------------------------------------
-- cube state

-- TODO: rename State -> Node

data Dim = X | Y | Z

data Col = White | Yellow | Blue | Green | Red | Orange deriving Show

data CornerOrientation = Xyz | Xzy | Yxz | Yzx | Zxy | Zyx deriving (Eq,Ord,Show)

data CornerPiece = WBR | WBO | WGR | WGO | YBR | YBO | YGR | YGO deriving (Eq,Ord,Show)

data Face = Front | Back | Up | Down | Left | Right deriving Show

type OC = (CornerPiece,CornerOrientation)

data State = State { ful, fur, fdl, fdr, bul, bur, bdl, bdr :: OC} deriving (Eq,Ord,Show)


distance :: State -> State -> Int
distance s1 s2 = do
  let State{ful=a,fur=b,fdl=c,fdr=d,bul=e,bur=f,bdl=g,bdr=h} = s1
  let State{ful=i,fur=j,fdl=k,fdr=l,bul=m,bur=n,bdl=o,bdr=p} = s2
  length [ () | False <- [(a==i), (b==j), (c==k), (d==l), (e==m), (f==n), (g==o), (h==p)] ]


solvedState :: State
solvedState = State
  { ful = (WBR, Yzx)
  , fur = (WBO, Yzx)
  , bul = (WGR, Yzx)
  , bur = (WGO, Yzx)
  , fdl = (YBR, Yzx)
  , fdr = (YBO, Yzx)
  , bdl = (YGR, Yzx)
  , bdr = (YGO, Yzx)
  }

clock :: Face -> State -> State
clock = \case
  Front -> \state@State{ful=a,fur=b,fdr=c,fdl=d} -> state {ful=z d,fur=z a,fdr=z b,fdl=z c}
  Back  -> \state@State{bur=a,bul=b,bdl=c,bdr=d} -> state {bur=z d,bul=z a,bdl=z b,bdr=z c}
  Up    -> \state@State{bul=a,bur=b,fur=c,ful=d} -> state {bul=y d,bur=y a,fur=y b,ful=y c}
  Down  -> \state@State{fdl=a,fdr=b,bdr=c,bdl=d} -> state {fdl=y d,fdr=y a,bdr=y b,bdl=y c}
  Left  -> \state@State{bul=a,ful=b,fdl=c,bdl=d} -> state {bul=x d,ful=x a,fdl=x b,bdl=x c}
  Right -> \state@State{fur=a,bur=b,bdr=c,fdr=d} -> state {fur=x d,bur=x a,bdr=x b,fdr=x c}
  where
    x (piece,orientation) = (piece,rotate X orientation)
    y (piece,orientation) = (piece,rotate Y orientation)
    z (piece,orientation) = (piece,rotate Z orientation)

rotate :: Dim -> CornerOrientation -> CornerOrientation
rotate = \case
  X -> \case Xyz -> Xzy; Xzy -> Xyz; Yxz -> Zxy; Yzx -> Zyx; Zxy -> Yxz; Zyx -> Yzx;
  Y -> \case Xyz -> Zyx; Xzy -> Zxy; Yxz -> Yzx; Yzx -> Yxz; Zxy -> Xzy; Zyx -> Xyz;
  Z -> \case Xyz -> Yxz; Xzy -> Yzx; Yxz -> Xyz; Yzx -> Xzy; Zxy -> Zyx; Zyx -> Zxy;

----------------------------------------------------------------------
-- picturing the cube...

_prettyState :: State -> [String]
_prettyState state = concat
  [ foldl1 beside [blank,  p Up ]
  , foldl1 beside [p Left, p Front, p Right, p Back]
  , foldl1 beside [blank,  p Down ]
  ] where
  blank = ["  ","  "]
  p = prettySquare . pictureFace state
  beside = zipWith $ \a b -> unwords [a,b]

prettySquare :: Square -> [String]
prettySquare (Square a b c d) = [[p a, p b],[p d, p c]] where p = prettyCol

prettyCol :: Col -> Char
prettyCol = head . show

data Square = Square Col Col Col Col -- square viewed clockwise as we look at the face
data Sticker = First | Second | Third

pictureFace :: State -> Face -> Square
pictureFace State {ful, fur, fdl, fdr, bul, bur, bdl, bdr} = \case
    Front -> square Z ful fur fdr fdl
    Back  -> square Z bur bul bdl bdr
    Up    -> square Y bul bur fur ful
    Down  -> square Y fdl fdr bdr bdl
    Left  -> square X bul ful fdl bdl
    Right -> square X fur bur bdr fdr
  where

square :: Dim -> OC -> OC -> OC -> OC -> Square
square q a b c d = Square (sticker q a) (sticker q b) (sticker q c) (sticker q d)

sticker :: Dim -> OC -> Col
sticker q (piece,orientation) = unstick (chooseSticker q orientation) piece

chooseSticker :: Dim -> CornerOrientation -> Sticker
chooseSticker = \case
  X -> \case Xyz -> f; Xzy -> f; Yxz -> s; Yzx -> t; Zxy -> s; Zyx -> t;
  Y -> \case Xyz -> s; Xzy -> t; Yxz -> f; Yzx -> f; Zxy -> t; Zyx -> s;
  Z -> \case Xyz -> t; Xzy -> s; Yxz -> t; Yzx -> s; Zxy -> f; Zyx -> f;
  where
    f = First; s = Second; t = Third

unstick :: Sticker -> CornerPiece -> Col
unstick = \case
  First  -> \case WBR -> w; WBO -> w; WGR -> w; WGO -> w; YBR -> y; YBO -> y; YGR -> y; YGO -> y;
  Second -> \case WBR -> b; WBO -> b; WGR -> g; WGO -> g; YBR -> b; YBO -> b; YGR -> g; YGO -> g;
  Third  -> \case WBR -> r; WBO -> o; WGR -> r; WGO -> o; YBR -> r; YBO -> o; YGR -> r; YGO -> o;
  where
    w = White; y = Yellow; b = Blue; g = Green; r = Red; o = Orange

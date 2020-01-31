
module Main (main) where

import Control.Monad (when)
import Data.Map (Map)
import Prelude hiding (Left,Right)
import System.Environment (getArgs)
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

--import Data.HashSet -- (Set)
--import qualified Data.Set as Set

----------------------------------------------------------------------
{- TODO: experiments...

- half turns as atomic moves: L2, R2.. etc
- full cube turns at atomic moved : X, Y, Z, X', Y', Z'

- only allow F/R/U - keeps cubie-bdl fixed in place, so getting 24x factor reduction in state space

-- strict annotation in cube state types

-- MAIN IDEA: dfs to find macro moves, which have reduced disturbance: 3,2,1

-}
----------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ "rubik: " <> show args

  let desc = parseArgs desc0 args
  print desc

  let seq = concat $ repeat [R,U,F,R,F',R',U',F,R',F,U,R,F'] -- TODO, random?

  let Desc{scrambleLength,atomicMoves} = desc
  let graph = mkGraph atomicMoves
  let scrambleSequence = take scrambleLength seq
  let scrambledState = foldl applyMove solvedState scrambleSequence

  print scrambleSequence
  putStrLn $ unlines (_prettyState scrambledState)

  (i,sol) <- viewSearch desc graph scrambledState

  putStrLn $ concat["Found solution: ", show sol, ", in ", show i, " steps."]
  let (Elem Path{inReverse=steps} _) = sol
  let final = foldl applyMove scrambledState (reverse steps)
  putStrLn $ "Check: " <> show (final == solvedState)
  putStrLn $ unlines (_prettyState final)

----------------------------------------------------------------------

data Heuristic = GH | JustG deriving Show

data Desc = Desc
  { scrambleLength :: Int
  , atomicMoves :: [Move]
  , seeSearchEvery :: Int
  , trackExpanded :: Bool
  , heuristic :: Heuristic
  } deriving Show

desc0 :: Desc
desc0 = Desc
  { scrambleLength = 10
  , atomicMoves = _fur
  , seeSearchEvery = 10000
  , trackExpanded = False
  , heuristic = GH
  }

parseArgs :: Desc -> [String] -> Desc
parseArgs desc = \case
  [] -> desc
  "-4":rest      -> parseArgs (desc { scrambleLength = 4 }) rest
  "-5":rest      -> parseArgs (desc { scrambleLength = 5 }) rest
  "-6":rest      -> parseArgs (desc { scrambleLength = 6 }) rest
  "-7":rest      -> parseArgs (desc { scrambleLength = 7 }) rest
  "-8":rest      -> parseArgs (desc { scrambleLength = 8 }) rest
  "-9":rest      -> parseArgs (desc { scrambleLength = 9 }) rest
  "-10":rest     -> parseArgs (desc { scrambleLength = 10 }) rest
  "-11":rest     -> parseArgs (desc { scrambleLength = 11 }) rest
  "-12":rest     -> parseArgs (desc { scrambleLength = 12 }) rest
  "-13":rest     -> parseArgs (desc { scrambleLength = 13 }) rest
  "--len":n:rest -> parseArgs (desc { scrambleLength = read n }) rest

  "--see":rest   -> parseArgs (desc { seeSearchEvery = 1 }) rest
  "--k":rest     -> parseArgs (desc { seeSearchEvery = 1000 }) rest
  "--10k":rest   -> parseArgs (desc { seeSearchEvery = 10000 }) rest
  "--100k":rest  -> parseArgs (desc { seeSearchEvery = 100000 }) rest
  "--mil":rest   -> parseArgs (desc { seeSearchEvery = 1000000 }) rest

  "--ex":rest    -> parseArgs (desc { trackExpanded = True }) rest
  "--bfs":rest   -> parseArgs (desc { heuristic = JustG }) rest

  "--f":rest     -> parseArgs (desc { atomicMoves = _f }) rest
  "--fu":rest    -> parseArgs (desc { atomicMoves = _fu }) rest
  "--fu2":rest   -> parseArgs (desc { atomicMoves = _fu2 }) rest
  "--fur2":rest  -> parseArgs (desc { atomicMoves = _fur2 }) rest

  "--fur":rest   -> parseArgs (desc { atomicMoves = _fur }) rest
  "--furA":rest   -> parseArgs (desc { atomicMoves = _fur ++ _fur' }) rest
  "--furH":rest   -> parseArgs (desc { atomicMoves = _fur ++ _fur2 }) rest
  "--furAH":rest   -> parseArgs (desc { atomicMoves = _fur ++ _fur2 ++ _fur' }) rest

  args -> error $ "parseArgs: " <> show args

----------------------------------------------------------------------
-- Heuristic

computeScoreGH :: Elem -> Score
computeScoreGH (Elem path state) = do
  let g = pathLength path
  let h = distance solvedState state
  Score (g + h) -- using path-length + distance-to-target heuristic

computeScoreG :: Elem -> Score
computeScoreG (Elem path _) = do
  let g = pathLength path
  Score g

computeScore :: Heuristic -> Elem -> Score
computeScore = \case
  GH -> computeScoreGH
  JustG -> computeScoreG

----------------------------------------------------------------------

viewSearch :: Desc -> Graph -> State -> IO (Int,Elem)
viewSearch desc@Desc{seeSearchEvery} graph scrambled = loop 0 (mkInitSS scrambled)
  where
    loop :: Int -> SS -> IO (Int,Elem)
    loop i ss = do
      when (i `mod` seeSearchEvery == 0) $ see i ss;
      case searchStep desc graph ss of
        Fail -> error $ "Search failed after " <> show i <> " steps."
        Success elem -> return (i,elem)
        Continue ss -> loop (i+1) ss

    see :: Int -> SS -> IO ()
    see i SS {focus, expanded=_x, frontier=_f
             , frontierSize=_fz
             } = do
      print (i,
             Set.size _x,
             focus,
             _frontierKeys _f,
             --_frontierStats _f,
             _fz -- always (b-1) * i, where b is the #links
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
mkGraph moves = Graph { links = \s -> map (\m -> (m, applyMove s m)) moves
                      , target = \s -> s == solvedState
                      }
----------------------------------------------------------------------
-- Frontier

newtype Frontier = Frontier (Map Score [Elem]) deriving (Show)

_frontierKeys :: Frontier -> [Score]
_frontierKeys (Frontier m) = Map.keys m

_frontierStats :: Frontier -> [(Score,Int)]
_frontierStats (Frontier m) = map (\(k,m) -> (k, length m)) (Map.toList m)

emptyFrontier :: Frontier
emptyFrontier = Frontier Map.empty

extendFrontier :: Frontier -> Score -> Elem -> Frontier
extendFrontier (Frontier m) key e = do
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
-- Search

data SS = SS -- Search State
  { focus :: !Elem
  , expanded :: !(Set State)
  , frontier :: !Frontier
  , frontierSize :: Int
  }

data SSK = Fail | Success Elem | Continue SS

mkInitSS :: State -> SS
mkInitSS scrambled = SS { focus = Elem emptyPath scrambled
                        , expanded = Set.empty
                        , frontier = emptyFrontier
                        , frontierSize = 0
                        }

searchStep :: Desc -> Graph -> SS -> SSK
searchStep
  Desc{ trackExpanded
      , heuristic
      }
  Graph{ links
       , target
       }
  SS{ focus = Elem path state
    , expanded = ex0
    , frontier = fr0
    , frontierSize = fz0
    } =

  if target state then Success (Elem path state) else do
    case pickUnexpanded fr1 of
      Nothing -> Fail
      Just (focus,frontier) -> do
        let frontierSize = length newElems + fz0 - 1
        Continue $ SS { focus
                      , expanded
                      , frontier
                      , frontierSize
                      }
  where
    expanded = if trackExpanded then Set.insert state ex0 else ex0

    fr1 = foldl extend fr0 newElems
      where extend fr e = extendFrontier fr (computeScore heuristic e) e

    newElems = [ Elem (postPend path m) n | (m,n) <- links state, n `notElem` expanded]

    pickUnexpanded :: Frontier -> Maybe (Elem,Frontier)
    pickUnexpanded frontier =
      case pickFrontier frontier of
        Nothing -> Nothing
        Just res@(Elem _ state, frontier') ->
          -- dont understand how this True can reduce the #steps !
          --if True || state `notElem` expanded then Just res else
          if state `notElem` expanded then Just res else
            pickUnexpanded frontier'

----------------------------------------------------------------------
-- moves

{-data Move = F  | B  | U  | D  | L  | R
          | F' | B' | U' | D' | L' | R' deriving (Show,Bounded,Enum)-}

data Move = F  | U  | R  | B  | D  | L
          | F' | U' | R' | B' | D' | L'
          | F2 | U2 | R2 | B2 | D2 | L2
  deriving (Show,Enum)

_f :: [Move]
_f = [F]

_fu :: [Move]
_fu = [F,U]

_fur :: [Move]
_fur = [F,U,R]

_fur' :: [Move]
_fur' = [F',U',R']

_fu2 :: [Move]
_fu2 = [F2,U2]

_fur2 :: [Move]
_fur2 = [F2,U2,R2]

applyMove :: State -> Move -> State
applyMove = flip $ \case
  F -> clock Front
  U -> clock Up
  R -> clock Right
  B -> clock Back
  D -> clock Down
  L -> clock Left

  F'-> anti Front
  U'-> anti Up
  R'-> anti Right
  B'-> anti Back
  D'-> anti Down
  L'-> anti Left

  F2 -> half Front
  U2 -> half Up
  R2 -> half Right
  B2 -> half Back
  D2 -> half Down
  L2 -> half Left

anti :: Face -> State -> State
anti face = clock face . clock face . clock face

half :: Face -> State -> State
half face = clock face . clock face


----------------------------------------------------------------------
-- cube state

-- TODO: rename State -> Node.. or better: Cube, or Cube2

data Dim = X | Y | Z

data Col = White | Yellow | Blue | Green | Red | Orange deriving Show

data CornerOrientation = Xyz | Xzy | Yxz | Yzx | Zxy | Zyx deriving (Eq,Ord,Show)

data CornerPiece = WBR | WBO | WGR | WGO | YBR | YBO | YGR | YGO deriving (Eq,Ord,Show)

data Face = Front | Back | Up | Down | Left | Right deriving Show

data OC = OC !CornerPiece !CornerOrientation deriving (Eq,Ord,Show)

data State = State { ful, fur, fdl, fdr, bul, bur, bdl, bdr :: !OC} deriving (Eq,Ord,Show)


distance :: State -> State -> Int
distance s1 s2 = do
  let State{ful=a,fur=b,fdl=c,fdr=d,bul=e,bur=f,bdl=g,bdr=h} = s1
  let State{ful=i,fur=j,fdl=k,fdr=l,bul=m,bur=n,bdl=o,bdr=p} = s2
  length [ () | False <- [(a==i), (b==j), (c==k), (d==l), (e==m), (f==n), (g==o), (h==p)] ]


solvedState :: State
solvedState = State
  { ful = oc (WBR, Yzx)
  , fur = oc (WBO, Yzx)
  , bul = oc (WGR, Yzx)
  , bur = oc (WGO, Yzx)
  , fdl = oc (YBR, Yzx)
  , fdr = oc (YBO, Yzx)
  , bdl = oc (YGR, Yzx)
  , bdr = oc (YGO, Yzx)
  } where
  oc (c,o) = OC c o

clock :: Face -> State -> State
clock = \case
  Front -> \state@State{ful=a,fur=b,fdr=c,fdl=d} -> state {ful=z d,fur=z a,fdr=z b,fdl=z c}
  Back  -> \state@State{bur=a,bul=b,bdl=c,bdr=d} -> state {bur=z d,bul=z a,bdl=z b,bdr=z c}
  Up    -> \state@State{bul=a,bur=b,fur=c,ful=d} -> state {bul=y d,bur=y a,fur=y b,ful=y c}
  Down  -> \state@State{fdl=a,fdr=b,bdr=c,bdl=d} -> state {fdl=y d,fdr=y a,bdr=y b,bdl=y c}
  Left  -> \state@State{bul=a,ful=b,fdl=c,bdl=d} -> state {bul=x d,ful=x a,fdl=x b,bdl=x c}
  Right -> \state@State{fur=a,bur=b,bdr=c,fdr=d} -> state {fur=x d,bur=x a,bdr=x b,fdr=x c}
  where
    x (OC piece orientation) = OC piece (rotate X orientation)
    y (OC piece orientation) = OC piece (rotate Y orientation)
    z (OC piece orientation) = OC piece (rotate Z orientation)

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
sticker q (OC piece orientation) = unstick (chooseSticker q orientation) piece

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

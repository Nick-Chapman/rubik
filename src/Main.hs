
module Main (main) where

--import Prelude hiding (Left,Right)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ "rubik: " <> show args
  putStrLn $ unlines (prettyState solvedState)
  putStrLn $ unlines (prettyState (clock F solvedState))
  putStrLn $ unlines (prettyState (clock B solvedState))
  putStrLn $ unlines (prettyState (clock U solvedState))
  putStrLn $ unlines (prettyState (clock D solvedState))
  putStrLn $ unlines (prettyState (clock L solvedState))
  putStrLn $ unlines (prettyState (clock R solvedState))

data Dim = X | Y | Z

data Col = White | Yellow | Blue | Green | Red | Orange deriving Show

data CornerOrientation = Xyz | Xzy | Yxz | Yzx | Zxy | Zyx

data CornerPiece = WBR | WBO | WGR | WGO | YBR | YBO | YGR | YGO

data Face = F | B | U | D | L | R

type OC = (CornerPiece,CornerOrientation)

data State = State { ful, fur, fdl, fdr, bul, bur, bdl, bdr :: OC}

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
  F -> \state@State{ful=a,fur=b,fdr=c,fdl=d} -> state {ful=z d,fur=z a,fdr=z b,fdl=z c}
  B -> \state@State{bur=a,bul=b,bdl=c,bdr=d} -> state {bur=z d,bul=z a,bdl=z b,bdr=z c}
  U -> \state@State{bul=a,bur=b,fur=c,ful=d} -> state {bul=y d,bur=y a,fur=y b,ful=y c}
  D -> \state@State{fdl=a,fdr=b,bdr=c,bdl=d} -> state {fdl=y d,fdr=y a,bdr=y b,bdl=y c}
  L -> \state@State{bul=a,ful=b,fdl=c,bdl=d} -> state {bul=x d,ful=x a,fdl=x b,bdl=x c}
  R -> \state@State{fur=a,bur=b,bdr=c,fdr=d} -> state {fur=x d,bur=x a,bdr=x b,fdr=x c}
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

prettyState :: State -> [String]
prettyState state = concat
  [ foldl1 beside [blank, p U ]
  , foldl1 beside [p L,   p F, p R,p B]
  , foldl1 beside [blank, p D ]
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
    F -> square Z ful fur fdr fdl
    B -> square Z bur bul bdl bdr
    U -> square Y bul bur fur ful
    D -> square Y fdl fdr bdr bdl
    L -> square X bul ful fdl bdl
    R -> square X fur bur bdr fdr
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

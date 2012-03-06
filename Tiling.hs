
module Tiling where

import Control.Applicative
import Data.List (inits)
import Data.Maybe
import qualified Data.Map as M

import Types

performLayoutOnSubviews :: InputView -> Rectangle Int -> [View]
performLayoutOnSubviews iv r = case ivLayout iv of
    Manual     -> map (performLayout Nothing) (ivSubviews iv)
    Horizontal -> tileSubviews iv (rectW r) (\(l, o) -> Rectangle o 0 l (rectH r))
    Vertical   -> tileSubviews iv (rectH r) (\(l, o) -> Rectangle 0 o (rectW r) l)

tileSubviews :: InputView -> Int -> ((Int, Int) -> Rectangle Int) -> [View]
tileSubviews iv space mkRect = zipWith performLayout tiledRects (ivSubviews iv)
    where tiledRects = [Just (mkRect (l, o)) | (l, o) <- lengthsAndOffsets]
          lengthsAndOffsets = linearTile' space
                                          (length $ ivSubviews iv)
                                          (M.fromList [])

performLayout :: Maybe (Rectangle Int) -> InputView -> View
performLayout maybeRect iv = View {
   vName = ivName iv,
   vClass = ivClass iv,
   vFrame = rect,
   vSubviews = performLayoutOnSubviews iv rect
}
    where rect = fromMaybe (Rectangle 0 0 0 0) $ ivFrame iv <|> maybeRect

-- for each (key, value)
-- element at |key| wants to have length |value|
type LinearConstraintSet = M.Map Int Int

-- precondition: tiling with given parameters is possible
-- tile |count| elements in |space| respecting |cs|
linearTile :: Int -> Int -> LinearConstraintSet -> [Int]
linearTile space count cs = map calculateLength [0..count-1]
    where calculateLength idx = fromMaybe residual (M.lookup idx cs)
          residual = (space - sum (M.elems cs)) `div` (count - M.size cs)

-- same as linearTile, but returns (length, offset) paris instead of lengths
linearTile' :: Int -> Int -> LinearConstraintSet -> [(Int, Int)]
linearTile' space count cs = zip ls $ map sum $ inits ls
    where ls = linearTile space count cs


module Tiling where

import Data.Maybe
import qualified Data.Map as M

import Types

data PartialFrame = PartialFrame {
    x :: Maybe Int,
    y :: Maybe Int,
    w :: Maybe Int,
    h :: Maybe Int
}

performMasterLayout :: InputView -> View
performMasterLayout iv = View {
   vName = ivName iv,
   vFrame = Rectangle 0 0 0 0,
   vSubviews = performLayoutOnSubviews iv
}

performLayoutOnSubviews :: InputView -> [View]
performLayoutOnSubviews iv = case ivLayout iv of
    Manual -> map (performLayout False) (ivSubviews iv)
    Horizontal -> []
    Vertical -> []

performLayout :: Bool -> InputView -> View
performLayout isMaster iv = View {
   vName = ivName iv,
   vFrame = if isMaster
            then Rectangle 0 0 0 0
            else fromJust $ ivFrame iv,
   vSubviews = performLayoutOnSubviews iv
}

tile :: Layout -> [PartialFrame] -> Maybe [Rectangle Int]
tile _ _ = Nothing

-- for each (key, value)
-- element at |key| wants to have length |value|
type LinearConstraintSet = M.Map Int Int

linearTile :: Int -> Int -> LinearConstraintSet -> [Int]
-- precondition: tiling with given parameters is possible
-- tile |count| elements in |space| respecting |cs|
linearTile space count cs = map calculateLength [0..count-1]
    where calculateLength idx = fromMaybe residual (M.lookup idx cs)
          residual = (space - sum (M.elems cs)) `div` (count - M.size cs)

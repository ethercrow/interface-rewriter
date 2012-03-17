{-# LANGUAGE OverloadedStrings #-}

module Validation (
    ValidationResult,
    validateView
) where

import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Maybe

import Types

type ValidationResult = Maybe L8.ByteString


validateView :: InputView -> ValidationResult
validateView view = msum $
                 [check view subview | check <- parentChildNodeChecks, subview <- subviews] ++ 
                 [check view | check <- singleNodeChecks] ++ 
                 map validateView subviews
                     where subviews = ivSubviews view


parentChildNodeChecks :: [InputView -> InputView -> ValidationResult]
parentChildNodeChecks = map (uncurry mkParentChildNodeCheck)
   [("child must have x when parent's layout is Manual",
        \ p c -> if ivLayout p == Manual then isJust (ivX c) else True),
    ("child must have y when parent's layout is Manual",
        \ p c -> if ivLayout p == Manual then isJust (ivY c) else True),
    ("child must have width when parent's layout is Manual",
        \ p c -> if ivLayout p == Manual then isJust (ivWidth c) else True),
    ("child must have height when parent's layout is Manual",
        \ p c -> if ivLayout p == Manual then isJust (ivHeight c) else True),

    ("child must have no width when parent's layout is Vertical",
        \ p c -> if ivLayout p == Vertical then isNothing (ivWidth c) else True),
    ("child must have no height when parent's layout is Horizontal",
        \ p c -> if ivLayout p == Horizontal then isNothing (ivHeight c) else True),

    ("child must have no y when parent's layout is not Manual",
        \ p c -> if ivLayout p /= Manual then isNothing (ivX c) else True),
    ("child must have no y when parent's layout is not Manual",
        \ p c -> if ivLayout p /= Manual then isNothing (ivY c) else True)
   ]

mkParentChildNodeCheck :: L8.ByteString -> (InputView -> InputView -> Bool) ->
                          InputView -> InputView -> ValidationResult
mkParentChildNodeCheck msg check parent child = if check parent child then Nothing else Just $
    L8.concat ["Pair (", ivName parent, ", ", ivName child, ") is invalid: ", msg]


singleNodeChecks :: [InputView -> ValidationResult]
singleNodeChecks = map (uncurry mkSingleNodeCheck)
   [("width must be positive",
        maybe True (>0) . ivWidth),
    ("height must be positive",
        maybe True (>0) . ivHeight)
   ]

mkSingleNodeCheck :: L8.ByteString -> (InputView -> Bool) ->
                     InputView -> ValidationResult
mkSingleNodeCheck msg check v = if check v then Nothing else Just $
    L8.concat [ivName v, "is invalid: ", msg]

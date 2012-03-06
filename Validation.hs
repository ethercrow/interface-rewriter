
module Validation (
    ValidationResult,
    validateView
) where

import Control.Monad
import Data.ByteString.Lazy.Char8(unpack)
import Data.Maybe

import Types

type ValidationResult = Maybe String

validateView :: InputView -> ValidationResult
validateView v = msum $
                 concatMap (\sv -> [checkFrame v sv, validateView sv]) svs
                 where svs = ivSubviews v

checkFrame :: InputView -> InputView -> ValidationResult
checkFrame parent child | ivLayout parent == Manual && isNothing (ivX child) =
                            Just $ unpack (ivName child) ++ " must have x"
                        | ivLayout parent == Manual && isNothing (ivY child) =
                            Just $ unpack (ivName child) ++ " must have y"
                        | ivLayout parent == Manual && isNothing (ivWidth child) =
                            Just $ unpack (ivName child) ++ " must have width"
                        | ivLayout parent == Manual && isNothing (ivHeight child) =
                            Just $ unpack (ivName child) ++ " must have height"

                        | ivLayout parent == Horizontal && isJust (ivX child) =
                            Just $ unpack (ivName child) ++ " must have no x"
                        | ivLayout parent == Horizontal && isJust (ivY child) =
                            Just $ unpack (ivName child) ++ " must have no y"
                        | ivLayout parent == Horizontal && isJust (ivHeight child) =
                            Just $ unpack (ivName child) ++ " must have no height"

                        | ivLayout parent == Vertical && isJust (ivX child) =
                            Just $ unpack (ivName child) ++ " must have no x"
                        | ivLayout parent == Vertical && isJust (ivY child) =
                            Just $ unpack (ivName child) ++ " must have no y"
                        | ivLayout parent == Vertical && isJust (ivWidth child) =
                            Just $ unpack (ivName child) ++ " must have no width"
checkFrame _ _ = Nothing


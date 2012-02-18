
module Validation (
    ValidationResult,
    validateView
) where

import InternalRepresentation
import Data.Maybe
import Control.Monad
import Data.ByteString.Lazy.Char8(unpack)

type ValidationResult = Maybe String

validateView :: View -> ValidationResult
validateView v = msum $ (map (checkFrame v) (viewSubviews v) ++ map validateView (viewSubviews v))
          

checkFrame :: View -> View -> ValidationResult
checkFrame parent child | viewLayout parent == Manual && isNothing (viewFrame child) =
                        Just $ unpack (viewName child) ++ " must have frame"
                 | viewLayout parent /= Manual && isJust (viewFrame child) =
                        Just $ unpack (viewName child) ++ " must have no frame"
checkFrame _ _ = Nothing
                 

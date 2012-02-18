
module Validation where

import InternalRepresentation
import Data.Maybe

-- maybe just use Maybe String?
data ValidationResult = OK
                      | ValidationError String

validateView :: View -> ValidationResult
validateView _ = OK

foo :: View -> View -> ValidationResult
foo parent child | viewLayout parent == Manual && isJust (viewFrame child) = OK
                 | viewLayout parent /= Manual && isNothing (viewFrame child) = OK
foo _ _ = ValidationError "dunno"
                 

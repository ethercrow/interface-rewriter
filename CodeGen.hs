{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module CodeGen (
    genHeader,
    genImplementation,
    TemplateMap
) where

import Data.Data
import Data.Maybe
import System.IO.Unsafe
import Text.Hastache
import Text.Hastache.Context
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Map as M

import Types

render :: L.ByteString -> MuContext IO -> L.ByteString
render templateString ctx = unsafePerformIO $ hastacheStr defaultConfig
                                    (encodeStr (L8.unpack templateString)) ctx

genHeader :: TemplateMap -> View -> L.ByteString
genHeader tm v = render template context
    where template = fromJust $ M.lookup "header.mu" tm
          context = mkGenericContext v

data Subview = Subview {
    subviewParent :: L.ByteString,
    subviewName :: L.ByteString,
    subviewFrame :: Rectangle Int
} deriving (Data, Typeable)

data ImplementationContext = ImplementationContext {
    master :: View,
    subviews :: [Subview]
} deriving (Data, Typeable)

genImplementation :: TemplateMap -> View -> L.ByteString
genImplementation tm v = render template context
    where template = fromJust $ M.lookup "implementation.mu" tm
          context = mkGenericContext ImplementationContext {
              master = v,
              subviews = flattenSubviews v
          }

flattenSubviews :: View -> [Subview]
flattenSubviews = flattenSubviews' True

flattenSubviews' :: Bool -> View -> [Subview]
flattenSubviews' isTopmost pv = map mkSubview (vSubviews pv) ++
                               concatMap (flattenSubviews' False) (vSubviews pv)
    where mkSubview v = Subview {
              subviewParent = if isTopmost then "self" else vName v,
              subviewName = vName v,
              subviewFrame = vFrame v
          }

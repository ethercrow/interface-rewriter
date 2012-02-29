{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module CodeGen (
    genHeader,
    genImplementation,
    TemplateMap
) where

import Text.Hastache
import Text.Hastache.Context
import Data.Maybe
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import System.IO.Unsafe
import Data.Data

import Types

type TemplateMap = M.Map String L.ByteString

render :: L.ByteString -> MuContext IO -> L.ByteString
render templateString ctx = unsafePerformIO $ hastacheStr defaultConfig
                                    (encodeStr (L8.unpack templateString)) ctx

genHeader :: TemplateMap -> InputView -> L.ByteString
genHeader tm v = render template context
    where template = fromJust $ M.lookup "header.mu" tm
          context = mkGenericContext v

data Rectangle t = Rectangle {
    x :: t,
    y :: t,
    w :: t,
    h :: t
} deriving (Data, Typeable)

fromArray :: [t] -> Rectangle t
fromArray xs = Rectangle {
    x = xs !! 0,
    y = xs !! 1,
    w = xs !! 2,
    h = xs !! 3
}

data Subview = Subview {
    subviewParent :: L.ByteString,
    subivName :: L.ByteString,
    subviewFrame :: Rectangle Int
} deriving (Data, Typeable)

data ImplementationContext = ImplementationContext {
    master :: InputView,
    subviews :: [Subview]
} deriving (Data, Typeable)

genImplementation :: TemplateMap -> InputView -> L.ByteString
genImplementation tm v = render template context
    where template = fromJust $ M.lookup "implementation.mu" tm
          context = mkGenericContext ImplementationContext {
              master = v,
              subviews = flattenSubviews v
          }

flattenSubviews :: InputView -> [Subview]
flattenSubviews = flattenSubviews' True

flattenSubviews' :: Bool -> InputView -> [Subview]
flattenSubviews' isTopmost pv = map mkSubview (ivSubviews pv) ++
                               concatMap (flattenSubviews' False) (ivSubviews pv)
    where mkSubview v = Subview {
              subviewParent = if isTopmost then "self" else ivName v,
              subivName = ivName v,
              subviewFrame = fromArray $ map fromJust [ivX v, ivY v, ivWidth v, ivHeight v]
          }

{-# LANGUAGE DeriveDataTypeable #-}

module InternalRepresentation where

import qualified Data.ByteString.Lazy as L
import Data.Data

type AttrKey = L.ByteString
type AttrValue = L.ByteString

data Rectangle = Rectangle {
    x :: Float,
    y :: Float,
    w :: Float,
    h :: Float
} deriving (Data, Typeable, Show)

data View = View {
    name :: L.ByteString,
    frame :: Rectangle,
    viewSubviews :: [View]
} deriving (Data, Typeable, Show)

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module InternalRepresentation where

import qualified Data.ByteString.Lazy as L
import Data.Vector
import Data.Data
import Data.Aeson
import Control.Applicative

type AttrKey = L.ByteString
type AttrValue = L.ByteString

data Rectangle t = Rectangle {
    x :: t,
    y :: t,
    w :: t,
    h :: t
} deriving (Data, Typeable)

fromArray :: Vector t -> Rectangle t
fromArray v = Rectangle (v!0) (v!1) (v!2) (v!3)

data Layout = Manual | Vertical | Horizontal
    deriving (Data, Typeable, Eq)

instance FromJSON Layout where
    parseJSON "manual" = return Manual
    parseJSON "vertical" = return Vertical
    parseJSON "horizontal" = return Horizontal
    parseJSON _ = fail "unknown layout"

data View = View {
    viewName :: L.ByteString,
    viewLayout :: Layout,
    viewFrame :: Maybe (Rectangle Int),
    viewWidth :: Maybe Int,
    viewHeight :: Maybe Int,
    viewSubviews :: [View]
} deriving (Data, Typeable)

instance FromJSON View where
    parseJSON (Object o) = View <$> o .: "name"
                                <*> o .:? "layout" .!= Manual

                                -- one fmap for Parser, second for Maybe
                                <*> fmap (fmap fromArray) (o .:? "frame")

                                <*> o .:? "width"
                                <*> o .:? "height"

                                <*> o .: "subviews"
    parseJSON _ = fail "expected object"


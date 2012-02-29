{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Data.ByteString.Lazy as L
import Data.Data
import Data.Aeson
import Control.Applicative

data Layout = Manual | Vertical | Horizontal
    deriving (Data, Typeable, Eq)

instance FromJSON Layout where
    parseJSON "manual" = return Manual
    parseJSON "vertical" = return Vertical
    parseJSON "horizontal" = return Horizontal
    parseJSON _ = fail "unknown layout"

data InputView = InputView {
    ivName :: L.ByteString,
    ivLayout :: Layout,

    ivX :: Maybe Int,
    ivY :: Maybe Int,
    ivWidth :: Maybe Int,
    ivHeight :: Maybe Int,

    ivSubviews :: [InputView]
} deriving (Data, Typeable)

instance FromJSON InputView where
    parseJSON (Object o) = InputView <$> o .: "name"
                                     <*> o .:? "layout" .!= Manual

                                     <*> o .:? "x"
                                     <*> o .:? "y"
                                     <*> o .:? "width"
                                     <*> o .:? "height"

                                     <*> o .: "subviews"
    parseJSON _ = fail "expected object"

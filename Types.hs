{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Control.Applicative
import Data.Aeson
import Data.Data
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M

data Layout = Manual | Vertical | Horizontal
    deriving (Data, Typeable, Eq)

-- view before layouting
data InputView = InputView {
    ivName :: L.ByteString,
    ivLayout :: Layout,

    ivX :: Maybe Int,
    ivY :: Maybe Int,
    ivWidth :: Maybe Int,
    ivHeight :: Maybe Int,

    ivSubviews :: [InputView]
} deriving (Data, Typeable)

ivFrame :: InputView -> Maybe (Rectangle Int)
ivFrame iv = do
    x' <- ivX iv
    y' <- ivY iv
    w' <- ivWidth iv
    h' <- ivHeight iv
    return $ Rectangle x' y' w' h'

-- view after layouting
data View = View {
    vName :: L.ByteString,
    vFrame :: Rectangle Int,
    vSubviews :: [View]
} deriving (Data, Typeable)

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

type TemplateMap = M.Map String L.ByteString

-- FromJSON instances
-- Extracting them from this file would trigger warning about orphaned instances
instance FromJSON InputView where
    parseJSON (Object o) = InputView <$> o .: "name"
                                     <*> o .:? "layout" .!= Manual

                                     <*> o .:? "x"
                                     <*> o .:? "y"
                                     <*> o .:? "width"
                                     <*> o .:? "height"

                                     <*> o .: "subviews"
    parseJSON _ = fail "expected object"

instance FromJSON Layout where
    parseJSON "manual" = return Manual
    parseJSON "vertical" = return Vertical
    parseJSON "horizontal" = return Horizontal
    parseJSON _ = fail "unknown layout"


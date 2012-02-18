{-# LANGUAGE OverloadedStrings #-}

module JSONReader where

import Data.Aeson (decode)
import Data.ByteString.Lazy as L

import InternalRepresentation

fromJSONFile :: FilePath -> IO (Maybe View)
fromJSONFile filename = do
    s <- L.readFile filename
    return $ decode s


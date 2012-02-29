{-# LANGUAGE OverloadedStrings #-}

module JSONReader (
    fromJSONFile
) where

import Data.Aeson (decode)
import Data.ByteString.Lazy as L

import Types

fromJSONFile :: FilePath -> IO (Maybe InputView)
fromJSONFile filename = do
    s <- L.readFile filename
    return $ decode s


{-# LANGUAGE OverloadedStrings #-}

module JSONReader (
    fromJSONFile
) where

import Data.Aeson (decode)
import Data.Maybe
import qualified Data.ByteString.Lazy as L

import Types

fromJSONFile :: FilePath -> IO (Either L.ByteString InputView)
fromJSONFile filename = do
    s <- L.readFile filename
    return $ maybe (Left "JSON parsing failed") (Right . fromJust) (decode s)

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Either.Unwrap
import Data.List
import Data.Maybe
import System.Environment
import System.Exit
import qualified Data.ByteString.Lazy.Char8 as L8

import CodeGen
import JSONReader
import Tiling
import UIReader
import Utils
import Validation

main :: IO ()
main = do
    maybeFilename <- fmap listToMaybe getArgs 

    unless (isJust maybeFilename) $ do
        putStrLn "Input file required"
        exitFailure

    let filename = fromJust maybeFilename

    let parse = if ".ui" `isSuffixOf` filename then fromUIFile else fromJSONFile

    eitherView <- parse $ fromJust maybeFilename

    whenLeft eitherView $ \ msg -> do
        L8.putStrLn $ L8.concat ["Parsing input file failed: ", msg]
        exitFailure

    let inputView = fromRight eitherView

    case validateView inputView of
        Just msg -> do
            L8.putStrLn $ L8.concat ["View validation error: ", msg]
            exitFailure
        _ -> return ()

    let view = performLayout Nothing inputView

    templates <- collectTemplates

    putStrLn "Header"
    putStrLn "======"
    L8.putStrLn $ genHeader templates view

    putStrLn "Implementation"
    putStrLn "=============="
    L8.putStrLn $ genImplementation templates view


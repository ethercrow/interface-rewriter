{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Maybe
import System.Environment
import System.Exit
import qualified Data.ByteString.Lazy.Char8 as L8

import JSONReader
import CodeGen
import Validation
import Tiling
import Utils

main :: IO ()
main = do
    maybeFilename <- fmap listToMaybe getArgs 

    unless (isJust maybeFilename) $ do
        putStrLn "Input file required"
        exitFailure

    maybeView <- fromJSONFile $ fromJust maybeFilename

    unless (isJust maybeView) $ do
        putStrLn "Parsing input file failed"
        exitFailure

    let inputView = fromJust maybeView

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

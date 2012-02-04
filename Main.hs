{-# LANGUAGE OverloadedStrings #-}

module Main where

import InternalRepresentation
import CodeGen
import System.Environment.Executable 
import System.FilePath
import System.Directory
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Data.List
import Control.Monad

main :: IO ()
main = do
    templates <- collectTemplates

    let subsubview = View "someButton" (Rectangle 10 10 30 30) []
        subview1 = View "shinyHeader" (Rectangle 100 100 300 100) [subsubview]
        subview2 = View "shinyFooter" (Rectangle 100 500 300 100) []
        view = View "ShinyView" (Rectangle 0 0 0 0) [subview1, subview2]

    putStrLn "Header"
    putStrLn "======"
    L.putStrLn $ genHeader templates view

    putStrLn "Implementation"
    putStrLn "=============="
    L.putStrLn $ genImplementation templates view

collectTemplates :: IO TemplateMap
collectTemplates = do
    programDirectory <- fmap fst splitExecutablePath
    let templateDirectory = programDirectory </> "templates"

    templateFilenames <- liftM (filter (isSuffixOf ".mu")) $
                         getDirectoryContents templateDirectory

    templates <- mapM (L.readFile . (templateDirectory </>)) templateFilenames
    return $ M.fromList $ zip templateFilenames templates
    

module Main where

import System.Environment
import System.Environment.Executable 
import System.FilePath
import System.Directory
import System.Exit
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Control.Monad

import JSONReader
import CodeGen
import Validation

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

    let view = fromJust maybeView

    case validateView view of
        ValidationError e -> do
            putStrLn $ "View validation error: " ++ e
            exitFailure
        _ -> return ()

    templates <- collectTemplates

    putStrLn "Header"
    putStrLn "======"
    L8.putStrLn $ genHeader templates view

    putStrLn "Implementation"
    putStrLn "=============="
    L8.putStrLn $ genImplementation templates view

collectTemplates :: IO TemplateMap
collectTemplates = do
    programDirectory <- fmap fst splitExecutablePath
    let templateDirectory = programDirectory </> "templates"

    templateFilenames <- liftM (filter (isSuffixOf ".mu")) $
                         getDirectoryContents templateDirectory

    templates <- mapM (L.readFile . (templateDirectory </>)) templateFilenames
    return $ M.fromList $ zip templateFilenames templates
    

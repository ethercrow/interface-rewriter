
module Utils where

import Control.Monad
import Data.List
import System.Directory
import System.Environment.Executable 
import System.FilePath
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M

import Types

collectTemplates :: IO TemplateMap
collectTemplates = do
    programDirectory <- fmap fst splitExecutablePath
    let templateDirectory = programDirectory </> "templates"

    templateFilenames <- liftM (filter (isSuffixOf ".mu")) $
                         getDirectoryContents templateDirectory

    templates <- mapM (L.readFile . (templateDirectory </>)) templateFilenames
    return $ M.fromList $ zip templateFilenames templates

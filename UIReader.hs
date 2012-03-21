{-# LANGUAGE OverloadedStrings #-}

module UIReader (
    fromUIFile
) where

import Text.ParserCombinators.Parsec
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

import Types

data ValueToken = Number Int
                | Layout Layout
                | QuotedString String

type Attribute = (String, ValueToken)

fromUIFile :: FilePath -> IO (Either L.ByteString InputView)
fromUIFile filename = do
    s <- readFile filename
    let eitherView = parse uiFile filename s
    return $ case eitherView of
        Left e -> Left $ L8.pack (show e)
        Right view -> Right view

uiFile :: GenParser Char st InputView
uiFile = do
    spaces
    result <- inputView
    spaces
    eof
    return result

inputView :: GenParser Char st InputView
inputView = do
    (viewClass, name) <- classAndName
    spaces
    _ <- char '{'
    spaces
    attrs <- many viewAttribute
    subviews <- many inputView
    _ <- char '}'
    spaces
    let result = InputView {
        ivName = L8.pack name,
        ivClass = L8.pack viewClass,
        ivLayout = getLayout attrs,
        ivX = getX attrs,
        ivY = getY attrs,
        ivWidth = getWidth attrs,
        ivHeight = getHeight attrs,
        ivSubviews = subviews
    }
    return result 

classAndName :: GenParser Char st (String, String)
classAndName = try longForm <|> shortForm
    where longForm = do
                     cl <- many1 letter
                     spaces
                     name <- many1 alphaNum 
                     return (cl, name)
          shortForm = do
                      name <- many1 alphaNum
                      return ("UIView", name)

viewAttribute :: GenParser Char st Attribute
viewAttribute = do
    spaces
    attribute <- numberAttribute <|> quotedStringAttribute <|> layoutAttribute
    spaces
    return attribute

quotedStringAttribute :: GenParser Char st Attribute
quotedStringAttribute = try $ do
    key <- many1 letter
    spaces
    _ <- char ':'
    spaces
    _ <- char '"'
    s <- many (noneOf "\"")
    _ <- char '"'
    return (key, QuotedString s)

numberAttribute :: GenParser Char st Attribute
numberAttribute = try $ do
    key <- many1 letter
    spaces
    _ <- char ':'
    spaces
    s <- many1 digit 
    return (key, Number (read s))

layoutAttribute :: GenParser Char st Attribute
layoutAttribute = try $ do
    key <- string "layout"
    spaces
    _ <- char ':'
    spaces
    layoutString <- string "manual" <|> string "horizontal" <|> string "vertical"

    let layoutValue = case layoutString of
            "manual" -> Manual
            "horizontal" -> Horizontal
            "vertical" -> Vertical

    return (key, Layout layoutValue)

getLayout :: [Attribute] -> Layout
getLayout attrs = case lookup "layout" attrs of
    Just (Layout l) -> l
    _               -> Manual


getX :: [Attribute] -> Maybe Int
getX = getMaybeInt "x"

getY :: [Attribute] -> Maybe Int
getY = getMaybeInt "y"

getWidth :: [Attribute] -> Maybe Int
getWidth = getMaybeInt "width"

getHeight :: [Attribute] -> Maybe Int
getHeight = getMaybeInt "height"


getMaybeInt :: String -> [Attribute] -> Maybe Int
getMaybeInt s attrs = case lookup s attrs of
    Just (Number x) -> Just x
    _               -> Nothing


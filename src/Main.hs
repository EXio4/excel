{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Attoparsec.ByteString.Char8 as P


import           Data.List
import           Data.Char
import           Control.Applicative
import           Data.Monoid

data Ins = Ins !ByteString [Ins]
         | Cell !ByteString
         | Op !Ins !ByteString !Ins
         | Literal !Integer
    deriving (Show,Eq)

data ExcelCode = Excel !Ins
    deriving (Show,Eq)

instruction :: P.Parser Ins 
instruction = do
    cmd <- P.takeWhile1 P.isAlpha_ascii
    P.takeWhile P.isSpace
    P.char '('
    pms <- P.sepBy ins (P.takeWhile P.isSpace >> P.string ";" >> P.takeWhile P.isSpace)
    P.takeWhile P.isSpace
    P.char ')'
    P.takeWhile P.isSpace
    return $ Ins cmd pms
    
expr :: P.Parser Ins
expr = do
    x <- cell <|> literal
    P.takeWhile P.isSpace
    op <- P.takeWhile1 (`elem` ("+-*<>=" :: String))
    P.takeWhile P.isSpace
    y <- cell <|> literal
    return (Op x op y)
    
cell :: P.Parser Ins     
cell = do
    cl <- C8.pack <$> P.many1 P.letter_ascii
    x  <- P.satisfy P.isDigit
    return $ Cell (cl <> C8.pack [x])

literal  :: P.Parser Ins 
literal = do
    sign <- maybe 1 (const (-1)) <$> optional (P.char '-')
    x <- P.takeWhile1 P.isDigit
    return $ Literal (sign * read (C8.unpack x))

ins :: P.Parser Ins 
ins = instruction <|> expr <|> cell <|> literal

prog :: P.Parser ExcelCode
prog = P.string "=" >> Excel <$> ins



exc :: ByteString
exc = "=SI(Y(A1<=B1;B1=2);42;-1)"

findCells :: Ins -> [ByteString]
findCells = nub . go where
    go (Cell x) = [x]
    go (Ins _ xs) = foldMap go xs
    go (Literal _) = []
    go (Op x _ y) = go x <> go y

compileHS :: ExcelCode -> String
compileHS (Excel ins) =
    let cells = findCells ins
        params = C8.unpack . C8.intercalate "," . map (C8.map toLower) $ cells
    in "f " <> params <> " = " <>
           go ins
    where go (Cell x) = map toLower $ C8.unpack x
          go (Op x "=" y) = conv x "==" y 
          go (Op x o y)  = conv x o y
          go (Literal n) = show n
          go (Ins "IF" [c,x,y]) = "if (" <> go c <> ") then (" <> go x <> ") else (" <> go y <> ")"
          go (Ins "AND" xs)  = "(" <> intercalate " && " (map go xs) <> ")"
          go (Ins "OR"  xs)  = "(" <> intercalate " || " (map go xs) <> ")"
          go (Ins "SI" [c,x,y]) = go (Ins "IF" [c,x,y])
          go (Ins "Y" xs) = go (Ins "AND" xs)
          go (Ins "O" xs) = go (Ins "OR"  xs)
          go x = error ("got unexpected: " <> show x)
          
          conv x o y = go x <> " " <> C8.unpack o <> " " <> go y

parseExcel :: ByteString -> Either String ExcelCode
parseExcel bs = P.parseOnly prog bs


main :: IO ()
main = putStrLn "Hello, Haskell!"

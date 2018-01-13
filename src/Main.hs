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
exc = "=IF(AND(A1<=B1;B1=2);42;-1)"

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

data Res = Expr | Stm Int

compileJS :: ExcelCode -> String
compileJS (Excel ins) =
    let cells = findCells ins
        params =  C8.unpack . C8.intercalate "," $ cells 
    in "function prog(" <> params <> ") {\n" <> go (Stm 0) ins <> "}"
    where
        sp n = replicate ((n+1) * 4) ' '
        ret k rest = sp k <> "return " <> rest <> ";\n"
        go :: Res -> Ins -> String
        go Expr    (Cell x) = C8.unpack x
        go (Stm k) (Cell x) = ret k $ C8.unpack x
        go Expr    (Op x "=" y) = conv x "==" y
        go Expr    (Op x  o  y) = conv x  o   y
        go (Stm k) (Op x  o  y) = ret k $ go Expr (Op x o y)
        go Expr    (Literal n) = show n
        go (Stm k) (Literal n) = ret k $ show n 
        go (Stm k) (Ins "IF" [c,x,y]) = sp k <> "if (" <> go Expr c <> ") {\n" <> 
                                                 go (Stm (k+1)) x  <>
                                                 sp k <> "} else {\n" <> 
                                                 go (Stm (k+1)) y  <>
                                                 sp k <> "}\n" 
        go Expr   (Ins "IF" [c,x,y]) = "(" <> go Expr c <> ") ? (" <> go Expr x <> ") : (" <> go Expr y <> ")"
        go Expr (Ins "AND" xs) = "(" <> intercalate " && " (map (go Expr) xs) <> ")"
        go Expr (Ins "OR"  xs) = "(" <> intercalate " || " (map (go Expr) xs) <> ")"
        go (Stm k) (Ins "AND" xs) = ret k $ go Expr (Ins "AND" xs)
        go (Stm k) (Ins "OR"  xs) = ret k $ go Expr (Ins "OR"  xs)
        go k (Ins "SI" xs) = go k (Ins "IF" xs)
        go k (Ins "Y"  xs) = go k (Ins "Y" xs)
        go k (Ins "O"  xs) = go k (Ins "O" xs)
        go _ err = error ("unexpected " ++ show err)
        
        conv x o y = "(" <> go Expr x <> ") " <> C8.unpack o <> " (" <> go Expr y <> ")"
        
parseExcel :: ByteString -> Either String ExcelCode
parseExcel bs = P.parseOnly prog bs


main :: IO ()
main = putStrLn "Hello, Haskell!"

{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow
import Data.List
import Data.Void
import System.Environment
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void String

type Name = String

data Expr
  = ATOM Atom
  | List [Expr]
  deriving (Eq, Read, Show)

data Atom
  = Int Int
  | Symbol Name
  deriving (Eq, Read, Show)

data JSExpr
  = JSInt Int
  | JSSymbol Name
  | JSBinOp JSBinOp JSExpr JSExpr
  | JSLambda [Name] JSExpr
  | JSFunCall JSExpr [JSExpr]
  | JSReturn JSExpr
  deriving (Eq, Show, Read)

type JSBinOp = String

type TranslationError = String

type Builtin = [Expr] -> Either TranslationError JSExpr

type Builtins = [(Name, Builtin)]

many1 :: Parser a -> Parser [a]
many1 parser = (:) <$> parser <*> many parser

parseName :: Parser Name
parseName = do
  c <- oneOf ['a' .. 'z']
  cs <- many $ oneOf $ ['a' .. 'z'] ++ "0123456789" ++ "_"
  pure (c : cs)

parseInt :: Parser Atom
parseInt = do
  sign <- optional $ char '-'
  num <- many1 $ oneOf "0123456789"
  let result = read $ maybe num (: num) sign
  pure $ Int result

parseSymbol :: Parser Atom
parseSymbol = fmap Symbol parseName

parseAtom :: Parser Atom
parseAtom = choice . (try <$>) $ [parseSymbol, parseInt]

parseList :: Parser [Expr]
parseList = between (char '(') (char ')') (try (space *> elements <* space))
  where
    elements =
      (space *> parseExpr <* space) `sepBy1` space

parseExpr :: Parser Expr
parseExpr = choice . (try <$>) $ [fmap List parseList, fmap ATOM parseAtom]

parseFile :: FilePath -> IO (Either (ParseErrorBundle String Void) Expr)
parseFile file = do
  input <- readFile file
  pure $ parse parseExpr "unknown" input

translateToJS :: Expr -> Either TranslationError JSExpr
translateToJS = \case
  ATOM (Symbol s) -> pure $ JSSymbol s
  ATOM (Int s) -> pure $ JSInt s
  List xs -> translateList xs

translateList :: [Expr] -> Either TranslationError JSExpr
translateList = \case
  [] -> Left ":("
  ATOM (Symbol s) : xs | Just f <- lookup s builtins -> f xs
  f : xs ->
    JSFunCall <$> translateToJS f <*> traverse translateToJS xs

builtins :: Builtins
builtins =
  [ ("lambda", translateLambda),
    ("let", translateLet),
    ("add", translateBinOp "add" "+"),
    ("mul", translateBinOp "mul" "*"),
    ("sub", translateBinOp "sub" "-"),
    ("div", translateBinOp "div" "/"),
    ("print", translatePrint)
  ]

fromSymbol :: Expr -> Either String Name
fromSymbol (ATOM (Symbol s)) = Right s
fromSymbol e = Left $ "cannot bind value to non symbol type: " ++ show e

translateLambda :: [Expr] -> Either TranslationError JSExpr
translateLambda = \case
  [List vars, body] -> do
    vars' <- traverse fromSymbol vars
    JSLambda vars' <$> (JSReturn <$> translateToJS body)
  vars ->
    Left $ "Syntax error: unexpected arguments for lambda:\n" ++ show (List $ ATOM (Symbol "lambda") : vars)

translateLet :: [Expr] -> Either TranslationError JSExpr
translateLet = \case
  [List binds, body] -> do
    (vars, vals) <- letParams binds
    vars' <- traverse fromSymbol vars
    JSFunCall . JSLambda vars' <$> (JSReturn <$> translateToJS body) <*> traverse translateToJS vals
  vars ->
    Left $ "Syntax error: unexpected arguments for let:\n" ++ show (List $ ATOM (Symbol "let") : vars)
  where
    letParams :: [Expr] -> Either TranslationError ([Expr], [Expr])
    letParams = \case
      [] -> pure ([], [])
      List [x, y] : xs -> ((x :) *** (y :)) <$> letParams xs
      x : _ -> Left $ "Unexpected argument in let list in expression:\n" ++ show x

translateBinOp :: Name -> Name -> [Expr] -> Either TranslationError JSExpr
translateBinOp f _ [] = Left $ "Syntax error: '" ++ f ++ "' expected at least 1 argument, got: 0"
translateBinOp _ _ [x] = translateToJS x
translateBinOp _ f list = foldl1 (JSBinOp f) <$> traverse translateToJS list

translatePrint :: [Expr] -> Either TranslationError JSExpr
translatePrint [expr] = JSFunCall (JSSymbol "console.log") . (: []) <$> translateToJS expr
translatePrint xs = Left $ "Syntax error. print expected 1 arguments, got: " ++ show (length xs)

printJSOp :: JSBinOp -> String
printJSOp op = op

printJSExpr :: JSExpr -> String
printJSExpr = \case
  JSInt i -> show i
  JSSymbol name -> name
  JSLambda vars expr ->
    "function" <> "(" <> intercalate ", " vars <> ") "
      <> "{" <> "\n"
      <> "  " <> printJSExpr expr
      <> "\n" <> "}"
  JSBinOp op e1 e2 -> "(" <> printJSExpr e1 <> " " <> printJSOp op <> " " <> printJSExpr e2 <> ")"
  JSFunCall f exprs ->
    "(" <> printJSExpr f <> ")("
      <> intercalate ", " (fmap printJSExpr exprs)
      <> ")"
  JSReturn expr -> "return " <> printJSExpr expr <> ";"

compileToAST :: FilePath -> IO (Either TranslationError JSExpr)
compileToAST file = do
  input <- readFile file
  let lisp = parse parseExpr "unknown" input
  case lisp of
    Right x -> pure $ translateToJS x
    _ -> pure $ Left ":("

main :: IO ()
main = do
  args <- getArgs  
  file <- getLine
  input <- readFile file
  let lisp = parse parseExpr "unknown" input
  case args of
    ["--write-file"] -> case lisp of
      Right x -> either putStrLn (writeFile "output.js") (printJSExpr <$> translateToJS x)
      Left e -> putStrLn "Parser error"
    ["--read-file"] -> case lisp of
      Right x -> either putStrLn putStrLn (printJSExpr <$> translateToJS x)
      Left e -> putStrLn "Parser error" 
    _ -> putStrLn "Invalid flag"

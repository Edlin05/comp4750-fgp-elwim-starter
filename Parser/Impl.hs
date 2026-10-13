module Parser.Impl (parse) where

import Control.Applicative (many, some, optional, (<|>))

import Parser.Precedence
import qualified Parser.Ast as Ast

import qualified Stream
import qualified Source
import qualified Scanner
import Scanner (Token(..), PosToken(..), TokenItr)
import Distribution.TestSuite (TestInstance(name))



-- Token helper functions
isIdentifier :: Token -> Bool
isIdentifier (Identifier _) = True
isIdentifier _ = False

isNumber :: Token -> Bool
isNumber (Number _) = True
isNumber _ = False

isUnaryOperator :: Token -> Bool
isUnaryOperator (Operator op) = isUnary op
isUnaryOperator _ = False



type Parser = Stream.Processor Scanner.TokenItr


parseToken :: Token -> Parser Token
parseToken = Stream.satisfy . (==)


requireToken :: Token -> Parser Token
requireToken token = Stream.require (parseToken token) ("expected: " ++ show token)


requirePrimary :: Parser Ast.Expression
requirePrimary = Stream.require parsePrimary "expected expression"


requireExpression :: Parser Ast.Expression
requireExpression = Stream.require parseExpr "expected expression"


requireStatement :: Parser Ast.Statement
requireStatement = Stream.require parseStatement "expected statement"


parseVariable :: Parser Ast.Expression
parseVariable = do
  token <- Stream.satisfy isIdentifier
  case token of
    Identifier name -> return $ Ast.Var name
    _ -> fail "expect Identifier"
  


parseNumber :: Parser Ast.Expression
parseNumber = do
  token <- Stream.satisfy isNumber
  case token of
    Number num -> return $ Ast.Num num
    _ -> fail "expect number"


parseUnary :: Parser Ast.Expression
parseUnary = do
  opToken <- Stream.satisfy isUnaryOperator
  case opToken of
    Operator op -> do
      expr <- requirePrimary
      return $ Ast.Unary op expr
    _ -> fail "needs a unary operator"


parseNested :: Parser Ast.Expression
parseNested = do
  parseToken LeftParen
  expr <- requireExpression
  requireToken RightParen
  return expr


parsePrimary :: Parser Ast.Expression
parsePrimary = parseNumber <|> parseVariable <|> parseUnary <|> parseNested


parseExpr :: Parser Ast.Expression
parseExpr = do
  expr <- parsePrimary
  moreOperators expr 0
  where
    moreOperators lhs minPrecedence = do
      next <- optional Stream.peekNext -- don't consume yet
      case next of
        Just (Operator op)
          | isBinary op && precedenceOf op > minPrecedence -> do
              Stream.getNext -- now we can consume
              rhs <- requirePrimary
              rhsExpanded <- moreOperators rhs (precedenceOf op)
              moreOperators (Ast.Binary op lhs rhsExpanded) minPrecedence
        _ -> return lhs


parsePrint :: Parser Ast.Statement
parsePrint = do
  parseToken Print
  expr <- requireExpression
  requireToken Semicolon
  return $ Ast.Print expr


parseScan :: Parser Ast.Statement
parseScan = do
  parseToken Scan
  token <- Stream.require (Stream.satisfy isIdentifier) "expected variable"
  requireToken Semicolon
  case token of 
    Identifier var -> return $ Ast.Scan var
    _ -> fail "expect var name"


parseAssignment :: Parser Ast.Statement
parseAssignment = do
  token <- Stream.satisfy isIdentifier
  requireToken Assign
  value <- requireExpression
  requireToken Semicolon
  case token of 
    Identifier var -> return $ Ast.Assign var value
    _ -> fail "expect var name"


parseBlock :: Parser Ast.Statement
parseBlock = do
  parseToken LeftBrace
  stmts <- many parseStatement
  requireToken RightBrace
  return $ Ast.Block stmts


parseIf :: Parser Ast.Statement
parseIf = do
  parseToken If
  requireToken LeftParen
  expr <- requireExpression
  requireToken RightParen
  thenStmt <- requireStatement
  maybeElse <- optional parseElse
  case maybeElse of
    Just elseStmt -> return $ Ast.IfElse expr thenStmt elseStmt
    Nothing       -> return $ Ast.If expr thenStmt
  where
    parseElse = do
      parseToken Else
      requireStatement


parseWhile :: Parser Ast.Statement
parseWhile = do
  parseToken While
  requireToken LeftParen
  expr <- requireExpression
  requireToken RightParen
  stmt <- requireStatement
  return $ Ast.While expr stmt


parseDoWhile :: Parser Ast.Statement
parseDoWhile = do
  parseToken Do
  stmt <- requireStatement
  requireToken While
  requireToken LeftParen
  expr <- requireExpression
  requireToken RightParen
  requireToken Semicolon
  return $ Ast.DoWhile stmt expr


parseStatement :: Parser Ast.Statement
parseStatement = parsePrint <|> parseScan <|> parseAssignment <|> parseBlock <|> parseIf <|> parseWhile <|> parseDoWhile


parseAll :: Parser Ast.Statement
parseAll = do
  statements <- many parseStatement
  eof <- Stream.getEof
  if eof
    then return $ Ast.Block statements
    else do
      token <- Stream.peekNext
      fail $ "unexpected: " ++ show token


parse :: [PosToken] -> Either (Source.Position, String) Ast.Statement
parse tokens = Stream.processResult parseAll (Scanner.begin tokens)

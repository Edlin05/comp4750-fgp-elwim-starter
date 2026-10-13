module Parser.Ast where

import Scanner (Operator)


data Expression 
  = Num Double
  | Var String
  | Unary Operator Expression
  | Binary Operator Expression Expression
  deriving(Show, Eq)

data Statement 
  = Print Expression
  | Scan String
  | Assign String Expression
  | If Expression Statement
  | IfElse Expression Statement Statement
  | Block [Statement]
  | While Expression Statement
  | DoWhile Statement Expression
  deriving (Show, Eq)
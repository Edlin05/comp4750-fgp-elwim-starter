module Interpreter (Context, evaluate, execute) where

import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad (foldM)

import Scanner.Token(Operator(..))
import qualified Parser.Ast as Ast
import MoreIO (promptNumber)

type Context = Map String Double


fmod :: Double -> Double -> Double
fmod x y = x - y * fromIntegral (truncate (x/y)::Integer)

evaluate :: Ast.Expression -> Context -> Double
evaluate (Ast.Num n) _ = n
evaluate (Ast.Var name) ctx = Map.findWithDefault 0.0 name ctx
evaluate (Ast.Unary op expr) ctx = 
    let val = evaluate expr ctx
    in case op of 
        Sub -> -val
        Add -> val
        Not -> if val == 0 then 1.0 else 0.0
        _ -> error $ "not a unary operator"
evaluate (Ast.Binary op e1 e2) ctx = 
    let v1 = evaluate e1 ctx
        v2 = evaluate e2 ctx
    in case op of 
        Add -> v1+v2 
        Sub -> v1-v2 
        Mul -> v1 * v2 
        Div -> v1 / v2 
        Mod -> fmod v1 v2 
        Eq -> if v1 == v2 then 1.0 else 0.0
        Ne -> if v1 /=v2 then 1.0 else 0.0
        Lt -> if v1<v2 then 1.0 else 0.0
        Le -> if v1<=v2 then 1.0 else 0.0
        Gt -> if v1>v2 then 1.0 else 0.0
        Ge -> if v1>=v2 then 1.0 else 0.0
        And -> if (v1 /= 0) && (v2 /= 0) then 1.0 else 0.0
        Or -> if (v1 /= 0) || (v2 /= 0) then 1.0 else 0.0
        _ -> error $ "not a binary operator"


execute :: Ast.Statement -> Context -> IO Context
execute (Ast.Print expr) ctx = do
    let val = evaluate expr ctx 
    print val 
    return ctx

execute (Ast.Scan varName) ctx = do
    val <- promptNumber varName 
    return $ Map.insert varName val ctx

execute (Ast.Assign varName expr) ctx = do
    let val = evaluate expr ctx 
    return $ Map.insert varName val ctx

execute (Ast.Block stmts) ctx = 
    foldM (\c stmt -> execute stmt c) ctx stmts 

execute (Ast.If cond thenStmt) ctx =
    if evaluate cond ctx /= 0
        then execute thenStmt ctx
        else return ctx 

execute (Ast.IfElse cond thenStmt elseStmt) ctx = 
    if evaluate cond ctx /= 0
        then execute thenStmt ctx
        else execute elseStmt ctx 

execute (Ast.While cond stmt) ctx = 
    if evaluate cond ctx /= 0
        then do 
            ctx' <- execute stmt ctx
            execute (Ast.While cond stmt) ctx'
        else return ctx 

execute (Ast.DoWhile stmt cond) ctx = do
    ctx' <- execute stmt ctx 
    if evaluate cond ctx' /= 0
        then execute (Ast.DoWhile stmt cond) ctx'
        else return ctx'

module Monkey.Interpreter exposing
  ( Answer(..), Value(..), Error(..), RuntimeError
  , run
  )


import Monkey.Environment as Env
import Monkey.Parser as P


type Answer
  = Void
  | Value Value


type Value
  = VNull
  | VNum Int
  | VBool Bool
  | VString String


type alias Env = Env.Environment String Value


type Error
  = SyntaxError P.Error
  | RuntimeError RuntimeError

type RuntimeError
  = NotImplemented


run : String -> Result Error Answer
run input =
  case P.parse input of
    Ok program ->
      let
        (_, result) =
          evalProgram program Env.empty
      in
      result
        |> Result.mapError RuntimeError

    Err err ->
      Err (SyntaxError err)


evalProgram : P.Program -> Env -> (Env, Result RuntimeError Answer)
evalProgram (P.Program stmts) =
  evalStmts stmts


evalStmts : List P.Stmt -> Env -> (Env, Result RuntimeError Answer)
evalStmts stmts env =
  case stmts of
    [] ->
      (env, Ok Void)

    [stmt] ->
      evalStmt stmt env

    stmt :: restStmts ->
      let
        (env1, _) =
          evalStmt stmt env
      in
      evalStmts restStmts env1


evalStmt : P.Stmt -> Env -> (Env, Result RuntimeError Answer)
evalStmt stmt env =
  case stmt of
    P.Let _ _ ->
      -- TODO: Eval Let.
      (env, Err NotImplemented)

    P.Return _ ->
      (env, Err NotImplemented)

    P.ExprStmt expr ->
      let
        (env1, result1) =
          evalExpr expr env
      in
      (env1, Result.map Value result1)


evalExpr : P.Expr -> Env -> (Env, Result RuntimeError Value)
evalExpr expr env =
  case expr of
    P.Var _ ->
      -- TODO: Eval Var.
      (env, Err NotImplemented)

    P.Num n ->
      (env, Ok (VNum n))

    P.Bool b ->
      (env, Ok (VBool b))

    P.String s ->
      (env, Ok (VString s))

    _ ->
      (env, Err NotImplemented)

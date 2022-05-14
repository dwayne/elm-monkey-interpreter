module Monkey.Interpreter exposing
  ( Answer(..), Value(..), Error(..), RuntimeError
  , run
  )


import Monkey.Parser as P


type Answer
  = Void
  | Value Value


type Value
  = VNull
  | VNum Int
  | VBool Bool
  | VString String


type Error
  = SyntaxError P.Error
  | RuntimeError RuntimeError

type RuntimeError
  = NotImplemented


run : String -> Result Error Answer
run input =
  case P.parse input of
    Ok program ->
      evalProgram program
        |> Result.mapError RuntimeError

    Err err ->
      Err (SyntaxError err)


evalProgram : P.Program -> Result RuntimeError Answer
evalProgram (P.Program stmts) =
  evalStmts stmts


evalStmts : List P.Stmt -> Result RuntimeError Answer
evalStmts stmts =
  case stmts of
    [] ->
      Ok Void

    [stmt] ->
      evalStmt stmt

    stmt :: restStmts ->
      evalStmt stmt
        |> Result.andThen
            (\_ ->
              evalStmts restStmts
            )


evalStmt : P.Stmt -> Result RuntimeError Answer
evalStmt stmt =
  case stmt of
    P.Let _ _ ->
      Err NotImplemented

    P.Return _ ->
      Err NotImplemented

    P.ExprStmt expr ->
      evalExpr expr
        |> Result.map Value


evalExpr : P.Expr -> Result RuntimeError Value
evalExpr expr =
  case expr of
    P.Var _ ->
      Err NotImplemented

    P.Num n ->
      Ok (VNum n)

    P.Bool b ->
      Ok (VBool b)

    P.String s ->
      Ok (VString s)

    _ ->
      Err NotImplemented

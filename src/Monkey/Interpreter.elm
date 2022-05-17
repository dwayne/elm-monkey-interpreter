module Monkey.Interpreter exposing
  ( Answer(..), Value(..), Error(..), RuntimeError(..)
  , run
  )


import Monkey.Environment as Env
import Monkey.Eval as Eval
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
  = IdentifierNotFound String
  | NotImplemented


type alias Eval err a = Eval.Eval Env err a


run : String -> Result Error Answer
run input =
  case P.parse input of
    Ok program ->
      evalProgram program
        |> Eval.run Env.empty
        |> Result.map Tuple.second
        |> Result.mapError RuntimeError

    Err err ->
      Err (SyntaxError err)


evalProgram : P.Program -> Eval RuntimeError Answer
evalProgram (P.Program stmts) =
  evalStmts stmts


evalStmts : List P.Stmt -> Eval RuntimeError Answer
evalStmts stmts =
  case stmts of
    [] ->
      Eval.succeed Void

    [stmt] ->
      evalStmt stmt

    stmt :: restStmts ->
      evalStmt stmt
        |> Eval.followedBy (evalStmts restStmts)


evalStmt : P.Stmt -> Eval RuntimeError Answer
evalStmt stmt =
  case stmt of
    P.Let identifier expr ->
      evalExpr expr
        |> Eval.andThen
            (\value ->
              Eval.getState
                |> Eval.andThen
                    (\env ->
                      Eval.replaceState (Env.extend identifier value env)
                        |> Eval.followedBy (Eval.succeed Void)
                    )
            )

    P.Return _ ->
      Eval.fail NotImplemented

    P.ExprStmt expr ->
      evalExpr expr
        |> Eval.map Value


evalExpr : P.Expr -> Eval RuntimeError Value
evalExpr expr =
  case expr of
    P.Var identifier ->
      Eval.getState
        |> Eval.andThen
            (\env ->
              case Env.lookup identifier env of
                Just value ->
                  Eval.succeed value

                Nothing ->
                  Eval.fail (IdentifierNotFound identifier)
            )

    P.Num n ->
      Eval.succeed (VNum n)

    P.Bool b ->
      Eval.succeed (VBool b)

    P.String s ->
      Eval.succeed (VString s)

    _ ->
      Eval.fail NotImplemented

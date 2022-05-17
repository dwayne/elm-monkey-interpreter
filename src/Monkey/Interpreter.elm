module Monkey.Interpreter exposing
  ( Answer(..), Value(..), Type(..), Error(..), RuntimeError(..)
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


type Type
  = TNull
  | TInt
  | TBool
  | TString


type alias Env = Env.Environment String Value


type Error
  = SyntaxError P.Error
  | RuntimeError RuntimeError

type RuntimeError
  = IdentifierNotFound String
  | TypeError Type Type
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

    P.Prefix op a ->
      evalExpr a
        |> Eval.andThen (evalPrefix op)

    _ ->
      Eval.fail NotImplemented


evalPrefix : P.UnaryOp -> Value -> Eval RuntimeError Value
evalPrefix op value =
  case op of
    P.Not ->
      computeNot value

    P.Negate ->
      computeNegate value


computeNot : Value -> Eval RuntimeError Value
computeNot value =
  case value of
    VNull ->
      Eval.succeed (VBool True)

    VBool b ->
      Eval.succeed (VBool <| not b)

    _ ->
      Eval.succeed (VBool False)


computeNegate : Value -> Eval RuntimeError Value
computeNegate value =
  expectInt value
    |> Eval.andThen (negate >> VNum >> Eval.succeed)


expectInt : Value -> Eval RuntimeError Int
expectInt value =
  case value of
    VNum n ->
      Eval.succeed n

    _ ->
      Eval.fail <| TypeError TInt (typeOf value)


typeOf : Value -> Type
typeOf value =
  case value of
    VNull ->
      TNull

    VNum _ ->
      TInt

    VBool _ ->
      TBool

    VString _ ->
      TString

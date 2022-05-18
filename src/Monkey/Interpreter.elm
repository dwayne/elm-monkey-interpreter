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
  | UnknownOperation String (List Type)
  | ZeroDivisionError
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
      Eval.succeed <| VNum n

    P.Bool b ->
      Eval.succeed <| VBool b

    P.String s ->
      Eval.succeed <| VString s

    P.Prefix op a ->
      evalExpr a
        |> Eval.andThen (evalPrefix op)

    P.Infix op a b ->
      Eval.andThen2 (evalInfix op) (evalExpr a) (evalExpr b)

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
      Eval.succeed <| VBool True

    VBool b ->
      Eval.succeed <| VBool <| not b

    _ ->
      Eval.succeed <| VBool False


computeNegate : Value -> Eval RuntimeError Value
computeNegate value =
  case value of
    VNum n ->
      Eval.succeed <| VNum <| negate n

    _ ->
      Eval.fail <| UnknownOperation "-" [typeOf value]


evalInfix : P.BinOp -> Value -> Value -> Eval RuntimeError Value
evalInfix op valueA valueB =
  case op of
    P.Equal ->
      computeEqual valueA valueB

    P.NotEqual ->
      computeNotEqual valueA valueB

    P.LessThan ->
      computeLessThan valueA valueB

    P.GreaterThan ->
      computeGreaterThan valueA valueB

    P.Add ->
      computeAdd valueA valueB

    P.Sub ->
      computeSub valueA valueB

    P.Mul ->
      computeMul valueA valueB

    P.Div ->
      computeDiv valueA valueB


computeEqual : Value -> Value -> Eval RuntimeError Value
computeEqual valueA valueB =
  Eval.succeed <| VBool <| valueEqual valueA valueB


computeNotEqual : Value -> Value -> Eval RuntimeError Value
computeNotEqual valueA valueB =
  Eval.succeed <| VBool <| not <| valueEqual valueA valueB


computeLessThan : Value -> Value -> Eval RuntimeError Value
computeLessThan valueA valueB =
  case (valueA, valueB) of
    (VNum a, VNum b) ->
      Eval.succeed <| VBool <| a < b

    _ ->
      Eval.fail <| UnknownOperation "<" [typeOf valueA, typeOf valueB]


computeGreaterThan : Value -> Value -> Eval RuntimeError Value
computeGreaterThan valueA valueB =
  case (valueA, valueB) of
    (VNum a, VNum b) ->
      Eval.succeed <| VBool <| a > b

    _ ->
      Eval.fail <| UnknownOperation ">" [typeOf valueA, typeOf valueB]


computeAdd : Value -> Value -> Eval RuntimeError Value
computeAdd valueA valueB =
  case (valueA, valueB) of
    (VNum a, VNum b) ->
      Eval.succeed <| VNum <| a + b

    (VString a, VString b) ->
      Eval.succeed <| VString <| a ++ b

    _ ->
      Eval.fail <| UnknownOperation "+" [typeOf valueA, typeOf valueB]


computeSub : Value -> Value -> Eval RuntimeError Value
computeSub valueA valueB =
  case (valueA, valueB) of
    (VNum a, VNum b) ->
      Eval.succeed <| VNum <| a - b

    _ ->
      Eval.fail <| UnknownOperation "-" [typeOf valueA, typeOf valueB]


computeMul : Value -> Value -> Eval RuntimeError Value
computeMul valueA valueB =
  case (valueA, valueB) of
    (VNum a, VNum b) ->
      Eval.succeed <| VNum <| a * b

    _ ->
      Eval.fail <| UnknownOperation "*" [typeOf valueA, typeOf valueB]


computeDiv : Value -> Value -> Eval RuntimeError Value
computeDiv valueA valueB =
  case (valueA, valueB) of
    (VNum a, VNum b) ->
      if b /= 0 then
        Eval.succeed <| VNum <| a // b
      else
        Eval.fail ZeroDivisionError

    _ ->
      Eval.fail <| UnknownOperation "/" [typeOf valueA, typeOf valueB]


valueEqual : Value -> Value -> Bool
valueEqual valueA valueB =
  case (valueA, valueB) of
    (VNull, VNull) ->
      True

    (VNum a, VNum b) ->
      a == b

    (VBool a, VBool b) ->
      a == b

    (VString a, VString b) ->
      a == b

    _ ->
      False


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

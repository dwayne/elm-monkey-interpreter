module Monkey.Interpreter exposing
  ( Answer(..), Value(..), Type(..), Error(..), RuntimeError(..)
  , run
  , answerToString
  )


import Array exposing (Array)
import Monkey.Environment as Env
import Monkey.Eval as Eval
import Monkey.Hash as Hash exposing (Hash)
import Monkey.Output as Output exposing (Output)
import Monkey.Parser as P


type Answer
  = Void
  | Return Value
  | Value Value


type Value
  = VNull
  | VNum Int
  | VBool Bool
  | VString String
  | VArray (Array Value)
  | VHash (Hash Value)
  | VFunction Closure
  | VBuiltInFunction BuiltInFunction
  | VReturn Value


type alias Closure =
  { params : List P.Id
  , body : P.Block
  , savedEnv : Env
  }


type alias BuiltInFunction = List Value -> Eval RuntimeError Value


type alias Env = Env.Environment P.Id Value


type Type
  = TNull
  | TInt
  | TBool
  | TString
  | TArray
  | THash
  | TFunction
  | TReturn Type


type Error
  = SyntaxError P.Error
  | RuntimeError RuntimeError

type RuntimeError
  = ArgumentError Int Int
  | IdentifierNotFound String
  | TypeError (List Type) Type
  | UnknownOperation String (List Type)
  | ZeroDivisionError


type alias Eval err a = Eval.Eval Env err a


run : String -> (Result Error Answer, List String)
run input =
  case P.parse input of
    Ok program ->
      evalProgram program
        |> Eval.run builtInFunctions
        |> Tuple.mapFirst (Result.map Tuple.second)
        |> Tuple.mapFirst (Result.mapError RuntimeError)
        |> Tuple.mapSecond Output.toList

    Err err ->
      ( Err (SyntaxError err)
      , []
      )


evalProgram : P.Program -> Eval RuntimeError Answer
evalProgram (P.Program stmts) =
  evalStmts stmts
    |> Eval.map
        (\answer ->
          case answer of
            Return value ->
              Value value

            _ ->
              answer
        )


evalStmts : List P.Stmt -> Eval RuntimeError Answer
evalStmts stmts =
  case stmts of
    [] ->
      Eval.succeed Void

    [stmt] ->
      evalStmt stmt

    stmt :: restStmts ->
      evalStmt stmt
        |> Eval.andThen
            (\answer ->
                case answer of
                  Return _ ->
                    Eval.succeed answer

                  _ ->
                    evalStmts restStmts
            )


evalStmt : P.Stmt -> Eval RuntimeError Answer
evalStmt stmt =
  case stmt of
    P.Let identifier expr ->
      case expr of
        P.Function _ _ ->
          Eval.getState
            |> Eval.andThen
                (Eval.replaceState << Env.extendRec identifier expr)
            |> Eval.followedBy (Eval.succeed Void)

        _ ->
          evalExpr expr
            |> Eval.andThen
                (\value ->
                  Eval.getState
                    |> Eval.andThen
                        (Eval.replaceState << Env.extend identifier value)
                    |> Eval.followedBy (Eval.succeed Void)
                )

    P.Return expr ->
      evalExpr expr
        |> Eval.map
            (\value ->
              case value of
                VReturn v ->
                  Return v

                _ ->
                  Return value
            )

    P.ExprStmt expr ->
      evalExpr expr
        |> Eval.map
            (\value ->
              case value of
                VReturn v ->
                  Return v

                _ ->
                  Value value
            )


evalExpr : P.Expr -> Eval RuntimeError Value
evalExpr expr =
  case expr of
    P.Var identifier ->
      Eval.getState
        |> Eval.andThen
            (\env ->
              case Env.lookup identifier env of
                Env.FoundValue value ->
                  Eval.succeed value

                Env.FoundExpr savedExpr ->
                  evalExpr savedExpr

                Env.NotFound ->
                  Eval.fail (IdentifierNotFound identifier)
            )

    P.Num n ->
      Eval.succeed <| VNum n

    P.Bool b ->
      Eval.succeed <| VBool b

    P.String s ->
      Eval.succeed <| VString s

    P.Array exprs ->
      evalExprs exprs
        |> Eval.map (VArray << Array.fromList)

    P.Hash kvExprs ->
      evalKVExprs kvExprs
        |> Eval.map (VHash << Hash.fromList)

    P.Prefix op a ->
      evalExpr a
        |> Eval.andThen (evalPrefix op)

    P.Infix op a b ->
      Eval.andThen2 (evalInfix op) (evalExpr a) (evalExpr b)

    P.Call f args ->
      Eval.andThen2 evalCall (evalExpr f) (evalExprs args)

    P.Index a i ->
      Eval.andThen2 evalIndex (evalExpr a) (evalExpr i)

    P.If condition thenBlock maybeElseBlock ->
      evalExpr condition
        |> Eval.andThen
            (\conditionVal ->
              if isTruthy conditionVal then
                evalBlock thenBlock
              else
                case maybeElseBlock of
                  Just elseBlock ->
                    evalBlock elseBlock

                  Nothing ->
                    Eval.succeed VNull
            )

    P.Function params body ->
      Eval.getState
        |> Eval.map (VFunction << Closure params body)


evalExprs : List P.Expr -> Eval RuntimeError (List Value)
evalExprs exprs =
  case exprs of
    [] ->
      Eval.succeed []

    expr :: restExprs ->
      evalExpr expr
        |> Eval.andThen
            (\value ->
                evalExprs restExprs
                  |> Eval.map ((::) (toNonReturnValue value))
            )

evalKVExprs : List (P.Expr, P.Expr) -> Eval RuntimeError (List (Hash.Key, Value))
evalKVExprs kvExprs =
  case kvExprs of
    [] ->
      Eval.succeed []

    (keyExpr, valueExpr) :: restKVExprs ->
      evalExpr keyExpr
        |> Eval.andThen
            (\possibleKey ->
                expectKey possibleKey
                  |> Eval.andThen
                      (\key ->
                          evalExpr valueExpr
                            |> Eval.andThen
                                (\value ->
                                    evalKVExprs restKVExprs
                                      |> Eval.map ((::) (key, toNonReturnValue value))
                                )
                      )
            )


evalBlock : P.Block -> Eval RuntimeError Value
evalBlock block =
  evalStmts block
    |> Eval.andThen
        (\answer ->
            case answer of
              Void ->
                Eval.succeed VNull

              Return value ->
                Eval.succeed (VReturn value)

              Value value ->
                Eval.succeed value
        )


evalPrefix : P.UnaryOp -> Value -> Eval RuntimeError Value
evalPrefix op v =
  let
    value =
      toNonReturnValue v
  in
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
evalInfix op vA vB =
  let
    valueA =
      toNonReturnValue vA

    valueB =
      toNonReturnValue vB
  in
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


evalCall : Value -> List Value -> Eval RuntimeError Value
evalCall v args =
  let
    value =
      toNonReturnValue v
  in
  case value of
    VFunction closure ->
      applyClosure closure args

    VBuiltInFunction builtInFunction ->
      builtInFunction args

    _ ->
      Eval.fail <| TypeError [TFunction] (typeOf value)


applyClosure : Closure -> List Value -> Eval RuntimeError Value
applyClosure { params, body, savedEnv } args =
  let
    numParams =
      List.length params

    numArgs =
      List.length args
  in
  if numParams == numArgs then
    evalBlock body
      |> Eval.withState (Env.extendMany (zip params args []) savedEnv)

  else
    Eval.fail <| ArgumentError numParams numArgs


zip : List a -> List b -> List (a, b) -> List (a, b)
zip xs ys revList =
  case (xs, ys) of
    ([], []) ->
      List.reverse revList

    (x :: restXs, y :: restYs) ->
      zip restXs restYs ((x, y) :: revList)

    _ ->
      []


evalIndex : Value -> Value -> Eval RuntimeError Value
evalIndex vA vB =
  let
    valueA =
      toNonReturnValue vA

    valueB =
      toNonReturnValue vB
  in
  case valueA of
    VArray array ->
      expectInt valueB
        |> Eval.andThen
            (\index ->
                Array.get index array
                  |> Maybe.withDefault VNull
                  |> Eval.succeed
            )

    VHash hash ->
      expectKey valueB
        |> Eval.andThen
            (\key ->
                Hash.lookup key hash
                  |> Maybe.withDefault VNull
                  |> Eval.succeed
            )

    _ ->
      Eval.fail <| TypeError [TArray, THash] (typeOf valueA)


expectInt : Value -> Eval RuntimeError Int
expectInt v =
  let
    value =
      toNonReturnValue v
  in
  case value of
    VNum n ->
      Eval.succeed n

    _ ->
      Eval.fail <| TypeError [TInt] (typeOf value)


expectKey : Value -> Eval RuntimeError Hash.Key
expectKey v =
  let
    value =
      toNonReturnValue v
  in
  case value of
    VNum n ->
      Eval.succeed <| Hash.KNum n

    VBool b ->
      Eval.succeed <| Hash.KBool b

    VString s ->
      Eval.succeed <| Hash.KString s

    _ ->
      Eval.fail <| TypeError [TInt, TBool, TString] (typeOf value)


isTruthy : Value -> Bool
isTruthy v =
  let
    value =
      toNonReturnValue v
  in
  case value of
    VNull ->
      False

    VBool False ->
      False

    _ ->
      True


toNonReturnValue : Value -> Value
toNonReturnValue value =
  case value of
    VReturn v ->
      toNonReturnValue v

    _ ->
      value


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

    VArray _ ->
      TArray

    VHash _ ->
      THash

    VFunction _ ->
      TFunction

    VBuiltInFunction _ ->
      TFunction

    VReturn v ->
      TReturn (typeOf v)


answerToString : Answer -> String
answerToString answer =
  case answer of
    Void ->
      ""

    Return value ->
      valueToString value

    Value value ->
      valueToString value


valueToString : Value -> String
valueToString value =
  case value of
    VNull ->
      "null"

    VNum n ->
      String.fromInt n

    VBool b ->
      if b then "true" else "false"

    VString s ->
      "\"" ++ s ++ "\""

    VArray array ->
      let
        inside =
          array
            |> Array.toList
            |> List.map valueToString
            |> String.join ", "
      in
      "[" ++ inside ++ "]"

    VHash hash ->
      let
        inside =
          hash
            |> Hash.toList
            |> List.map
                (\(k, v) ->
                    Hash.keyToString k ++ ": " ++ valueToString v
                )
            |> String.join ", "
      in
      "{" ++ inside ++ "}"

    VFunction _ ->
      "<function>"

    VBuiltInFunction _ ->
      "<built-in function>"

    VReturn v ->
      "return " ++ valueToString v


builtInFunctions : Env
builtInFunctions =
  [ ("len", builtInLen)
  , ("first", builtInFirst)
  , ("last", builtInLast)
  , ("rest", builtInRest)
  , ("push", builtInPush)
  , ("puts", builtInPuts)
  ]
  |> List.map (Tuple.mapSecond VBuiltInFunction)
  |> Env.fromList


builtInLen : BuiltInFunction
builtInLen args =
  case args of
    [VString s] ->
      Eval.succeed <| VNum <| String.length s

    [VArray a] ->
      Eval.succeed <| VNum <| Array.length a

    [arg] ->
      Eval.fail <| TypeError [TString, TArray] (typeOf arg)

    _ ->
      Eval.fail <| ArgumentError 1 (List.length args)


builtInFirst : BuiltInFunction
builtInFirst args =
  case args of
    [VArray a] ->
      Array.get 0 a
        |> Maybe.withDefault VNull
        |> Eval.succeed

    [arg] ->
      Eval.fail <| TypeError [TArray] (typeOf arg)

    _ ->
      Eval.fail <| ArgumentError 1 (List.length args)


builtInLast : BuiltInFunction
builtInLast args =
  case args of
    [VArray a] ->
      Array.get (Array.length a - 1) a
        |> Maybe.withDefault VNull
        |> Eval.succeed

    [arg] ->
      Eval.fail <| TypeError [TArray] (typeOf arg)

    _ ->
      Eval.fail <| ArgumentError 1 (List.length args)


builtInRest : BuiltInFunction
builtInRest args =
  case args of
    [VArray a] ->
      let
        rest =
          Array.toList >> List.tail >> Maybe.map Array.fromList
      in
      rest a
        |> Maybe.map VArray
        |> Maybe.withDefault VNull
        |> Eval.succeed

    [arg] ->
      Eval.fail <| TypeError [TArray] (typeOf arg)

    _ ->
      Eval.fail <| ArgumentError 1 (List.length args)


builtInPush : BuiltInFunction
builtInPush args =
  case args of
    [VArray a, value] ->
      Eval.succeed <| VArray <| Array.push value a

    [arg, _] ->
      Eval.fail <| TypeError [TArray] (typeOf arg)

    _ ->
      Eval.fail <| ArgumentError 2 (List.length args)


builtInPuts : BuiltInFunction
builtInPuts args =
  case args of
    [] ->
      Eval.succeed VNull

    (arg :: restArgs) ->
      printValue arg
        |> Eval.followedBy (builtInPuts restArgs)


printValue : Value -> Eval RuntimeError ()
printValue = Eval.print valueToString

module Monkey.Eval exposing
  ( Eval
  , run
  , succeed, fail
  , getState, replaceState
  , map
  , andThen, followedBy
  )


type Eval state err a
  = Eval (state -> Step state err a)


type Step state err a
  = Good state a
  | Bad err


run : state -> Eval state err a -> Result err (state, a)
run s0 (Eval st) =
  case st s0 of
    Good s1 a ->
      Ok (s1, a)

    Bad err ->
      Err err


succeed : a -> Eval state err a
succeed a =
  Eval (\s -> Good s a)


fail : err -> Eval state err a
fail err =
  Eval (\_ -> Bad err)


getState : Eval state err state
getState =
  Eval (\s -> Good s s)


replaceState : state -> Eval state err ()
replaceState s =
  Eval (\_ -> Good s ())


map : (a -> b) -> Eval state err a -> Eval state err b
map f (Eval st) =
  Eval
    (\s0 ->
      case st s0 of
        Good s1 a ->
          Good s1 (f a)

        Bad err ->
          Bad err
    )


andThen : (a -> Eval state err b) -> Eval state err a -> Eval state err b
andThen f (Eval st0) =
  Eval
    (\s0 ->
      case st0 s0 of
        Good s1 a ->
          case f a of
            Eval st1 ->
              st1 s1

        Bad err ->
          Bad err
    )


followedBy : Eval state err b -> Eval state err a -> Eval state err b
followedBy eval2 eval1 =
  eval1
    |> andThen (\_ -> eval2)

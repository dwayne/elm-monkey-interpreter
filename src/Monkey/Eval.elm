module Monkey.Eval exposing
  ( Eval
  , run
  , succeed, fail
  , getState, replaceState, withState
  , print
  , map
  , andThen, andThen2, followedBy
  )


import Monkey.Output as Output exposing (Output)


type Eval state err a
  = Eval ((state, Output) -> Step state err a)


type Step state err a
  = Good state Output a
  | Bad Output err


run : state -> Eval state err a -> (Result err (state, a), Output)
run s0 (Eval st) =
  case st (s0, Output.empty) of
    Good s1 output a ->
      ( Ok (s1, a)
      , output
      )

    Bad output err ->
      ( Err err
      , output
      )


succeed : a -> Eval state err a
succeed a =
  Eval (\(s, o) -> Good s o a)


fail : err -> Eval state err a
fail err =
  Eval (\(_, o) -> Bad o err)


getState : Eval state err state
getState =
  Eval (\(s, o) -> Good s o s)


replaceState : state -> Eval state err ()
replaceState s =
  Eval (\(_, o) -> Good s o ())


withState : state -> Eval state err a -> Eval state err a
withState newState (Eval st) =
  Eval
    (\(oldState, o0) ->
      case st (newState, o0) of
        Good _ o1 a ->
          Good oldState o1 a

        Bad o1 err ->
          Bad o1 err
    )
  -- Alternatively, we can implement it as follows:
  --
  -- getState
  --   |> andThen
  --       (\oldState ->
  --           replaceState newState
  --             |> followedBy eval
  --             |> andThen
  --                 (\value ->
  --                     replaceState oldState
  --                       |> followedBy (succeed value)
  --                 )
  --       )


print : (a -> String) -> a -> Eval state err ()
print toString a =
  Eval (\(s, o) -> Good s (Output.print toString a o) ())


map : (a -> b) -> Eval state err a -> Eval state err b
map f (Eval st) =
  Eval
    (\(s0, o0) ->
      case st (s0, o0) of
        Good s1 o1 a ->
          Good s1 o1 (f a)

        Bad o1 err ->
          Bad o1 err
    )


andThen : (a -> Eval state err b) -> Eval state err a -> Eval state err b
andThen f (Eval st0) =
  Eval
    (\(s0, o0) ->
      case st0 (s0, o0) of
        Good s1 o1 a ->
          case f a of
            Eval st1 ->
              st1 (s1, o1)

        Bad o1 err ->
          Bad o1 err
    )


andThen2 : (a -> b -> Eval state err c) -> Eval state err a -> Eval state err b -> Eval state err c
andThen2 f eval1 eval2 =
  eval1
    |> andThen
        (\a ->
          eval2
            |> andThen
                (\b ->
                  f a b
                )
        )


followedBy : Eval state err b -> Eval state err a -> Eval state err b
followedBy eval2 eval1 =
  eval1
    |> andThen (\_ -> eval2)

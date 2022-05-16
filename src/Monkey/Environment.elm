module Monkey.Environment exposing
  ( Environment
  , empty, lookup, extend
  )


type Environment k v
  = Empty
  | Single k v (Environment k v)


empty : Environment k v
empty = Empty


lookup : k -> Environment k v -> Maybe v
lookup searchKey env =
  case env of
    Empty ->
      Nothing

    Single key value nextEnv ->
      if searchKey == key then
        Just value
      else
        lookup searchKey nextEnv


extend : k -> v -> Environment k v -> Environment k v
extend k v env = Single k v env

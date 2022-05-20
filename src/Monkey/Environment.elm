module Monkey.Environment exposing
  ( Environment
  , empty, fromList

  , LookupResult(..)
  , lookup

  , extend, extendRec, extendMany
  )


import Monkey.Parser as P


type Environment k v
  = Empty
  | Single k v (Environment k v)
  | SingleRec k P.Expr (Environment k v)
  | Many (List (k, v)) (Environment k v)


empty : Environment k v
empty = Empty


fromList : List (k, v) -> Environment k v
fromList assocs = Many assocs Empty


type LookupResult v
  = NotFound
  | FoundValue v
  | FoundExpr P.Expr


lookup : k -> Environment k v -> LookupResult v
lookup searchKey env =
  case env of
    Empty ->
      NotFound

    Single key value nextEnv ->
      if searchKey == key then
        FoundValue value
      else
        lookup searchKey nextEnv

    SingleRec key expr nextEnv ->
      if searchKey == key then
        FoundExpr expr
      else
        lookup searchKey nextEnv

    Many assocs nextEnv ->
      case assocsLookup searchKey assocs of
        Just value ->
          FoundValue value

        Nothing ->
          lookup searchKey nextEnv


assocsLookup : k -> List (k, v) -> Maybe v
assocsLookup searchKey assocs =
  case assocs of
    [] ->
      Nothing

    (key, value) :: restAssocs ->
      if searchKey == key then
        Just value
      else
        assocsLookup searchKey restAssocs


extend : k -> v -> Environment k v -> Environment k v
extend = Single


extendRec : k -> P.Expr -> Environment k v -> Environment k v
extendRec = SingleRec


extendMany : List (k, v) -> Environment k v -> Environment k v
extendMany = Many

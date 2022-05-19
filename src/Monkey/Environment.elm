module Monkey.Environment exposing
  ( Environment
  , empty
  , lookup
  , extend, extendMany
  )


type Environment k v
  = Empty
  | Single k v (Environment k v)
  | Many (List (k, v)) (Environment k v)


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

    Many assocs nextEnv ->
      case assocsLookup searchKey assocs of
        Just value ->
          Just value

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


extendMany : List (k, v) -> Environment k v -> Environment k v
extendMany = Many

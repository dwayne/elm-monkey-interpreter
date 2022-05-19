module Monkey.Hash exposing
  ( Hash, Key(..)
  , fromList
  , toList
  , lookup
  , keyToString
  )


type Hash v
  = Hash (List (Key, v))

type Key
  = KNum Int
  | KBool Bool
  | KString String


fromList : List (Key, v) -> Hash v
fromList = Hash


toList : Hash v -> List (Key, v)
toList (Hash assocs) = assocs


lookup : Key -> Hash v -> Maybe v
lookup searchKey (Hash assocs) =
  let
    helper list =
      case list of
        [] ->
          Nothing

        (key, value) :: rest ->
          if searchKey == key then
            Just value

          else
            helper rest
  in
  helper assocs


keyToString : Key -> String
keyToString key =
  case key of
    KNum n ->
      String.fromInt n

    KBool b ->
      if b then "true" else "false"

    KString s ->
      "\"" ++ s ++ "\""

module Monkey.Hash exposing
  ( Hash, Key(..)
  , fromList
  , lookup
  )


type Hash v
  = Hash (List (Key, v))

type Key
  = KNum Int
  | KBool Bool
  | KString String


fromList : List (Key, v) -> Hash v
fromList =
  Hash


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

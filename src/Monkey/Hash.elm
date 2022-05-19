module Monkey.Hash exposing
  ( Hash, Key(..)
  , fromList
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

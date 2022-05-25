module Monkey.Output exposing
  ( Output
  , empty
  , print
  , toList
  )


type Output
  = Output (List String)


empty : Output
empty = Output []


print : (a -> String) -> a -> Output -> Output
print toString a (Output buffer) =
  Output (toString a :: buffer)


toList : Output -> List String
toList (Output buffer) = List.reverse buffer

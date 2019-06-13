module Helpers exposing (ident_to_tokens)

import Types exposing (..)


ident_to_tokens : String -> List Token
ident_to_tokens s =
    String.toList s |> List.map Char

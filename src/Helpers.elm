module Helpers exposing (ident_to_tokens)

import Types exposing (..)


ident_to_tokens : ContextType -> String -> List Token
ident_to_tokens e s =
    String.toList s |> List.map (Char e)

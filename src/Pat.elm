module Pat exposing (to_tokens)

import Helpers exposing (..)
import Types exposing (..)


to_tokens : Pat -> List Token
to_tokens p =
    case p of
        PatHole ->
            [ HoleToken P ]

        PatVar s ->
            ident_to_tokens P s

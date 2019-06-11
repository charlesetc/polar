module Cursor exposing (insert_cursor)

import Types exposing (..)


insert_cursor : Cursor -> List Token -> List Token
insert_cursor i ts =
    let
        ( a, b ) =
            ( List.take i ts, List.drop i ts )
    in
    a ++ [ Cursor ] ++ b

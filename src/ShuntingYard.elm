module ShuntingYard exposing
    ( Ast_or_op(..)
    , SList
    , shunting_yard
    , slist_of_sequence
    )

import Types exposing (..)


precedence_of_op : Op -> Int
precedence_of_op o =
    case o of
        Space ->
            3

        Times ->
            2

        Divide ->
            2

        Plus ->
            1

        Minus ->
            1


type Ast_or_op
    = A Ast
    | O Op


type alias SList =
    List Ast_or_op


shunting_yard : List Op -> SList -> SList
shunting_yard ostack slist =
    --! change the order
    case ( slist, ostack ) of
        ( (A a) :: rest, _ ) ->
            A a :: shunting_yard ostack rest

        ( (O op) :: rest, [] ) ->
            shunting_yard [ op ] rest

        ( (O a) :: srest, b :: orest ) ->
            if precedence_of_op a > precedence_of_op b then
                shunting_yard (a :: b :: orest) srest

            else
                O b :: shunting_yard (a :: orest) srest

        ( [], op :: rest ) ->
            O op :: shunting_yard rest []

        ( [], [] ) ->
            []


slist_of_sequence : Sequence -> SList
slist_of_sequence sequence =
    case sequence of
        End a ->
            [ A a ]

        Cons ast op seq ->
            A ast :: O op :: slist_of_sequence seq



-- sequence_of_slist : SList -> Sequence
-- sequence_of_slist slist =
--     case slist of
--         [ A a ] ->
--             End a
--         (A ast) :: (O op) :: rest ->
--             Cons ast op (sequence_of_slist rest)
--         _ ->
--             Debug.log "whoops! malformed slist" End (Var "Error - malformed slist")
-- shunting_yard_on_sequence : Sequence -> Sequence
-- shunting_yard_on_sequence sequence =
--     slist_of_sequence sequence
--         |> shunting_yard []
--         |> sequence_of_slist

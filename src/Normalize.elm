module Normalize exposing (normalize_parens)

import Helpers exposing (..)
import Types exposing (..)


type TokenTree
    = Token Token
    | Paren (List TokenTree)
    | Error String


print_token_tree : TokenTree -> List Token
print_token_tree tree =
    case tree of
        Token t ->
            [ t ]

        Paren [] ->
            []

        Paren (a :: []) ->
            print_token_tree a

        Paren (a :: (Token Cursor) :: []) ->
            print_token_tree a ++ [ Cursor ]

        Paren ts ->
            [ OpenParen ] ++ List.concatMap print_token_tree ts ++ [ CloseParen ]

        Error s ->
            "error: " ++ s |> ident_to_tokens


parse_token_tree : List TokenTree -> List Token -> ( TokenTree, List Token )
parse_token_tree stack tokens =
    case tokens of
        OpenParen :: rest ->
            let
                ( tree, ts ) =
                    parse_token_tree [] rest
            in
            parse_token_tree (tree :: stack) ts

        CloseParen :: rest ->
            ( Paren (List.reverse stack), rest )

        a :: rest ->
            parse_token_tree (Token a :: stack) rest

        [] ->
            ( Paren stack, [] )


normalize_parens : List Token -> List Token
normalize_parens tokens =
    let
        ( tree, ts ) =
            parse_token_tree [] tokens

        _ =
            Debug.log "started with tokens, got tree" ( tokens, tree )

        _ =
            if not (List.isEmpty ts) then
                Debug.log "extra tokens while parsing" ts

            else
                []
    in
    print_token_tree tree

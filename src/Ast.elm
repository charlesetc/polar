module Ast exposing (parse_ast, to_tokens, view_ast)

import Cursor
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Token
import Types exposing (..)


to_tokens : Ast -> List Token
to_tokens ast =
    case ast of
        App a b ->
            [ OpenParen ] ++ to_tokens a ++ [ Space ] ++ to_tokens b ++ [ CloseParen ]

        Abs v body ->
            [ Lambda, Ident v, Dot ] ++ to_tokens body

        Var v ->
            [ Ident v ]

        Const c ->
            [ ConstantToken c ]

        Hole ->
            [ HoleToken ]


view_ast : Ast -> Maybe Cursor -> Html Msg
view_ast ast cursor =
    to_tokens ast
        |> (case cursor of
                Just c ->
                    Cursor.insert_cursor c

                Nothing ->
                    \x -> x
           )
        |> List.map Token.view_token
        |> span [ class "ast" ]


parse_ast : List Token -> ( Ast, List Token )
parse_ast tokens =
    case tokens of
        (Ident v) :: ts ->
            ( Var v, ts )

        Lambda :: (Ident name) :: Dot :: ts ->
            let
                ( body, rest ) =
                    parse_ast ts
            in
            ( Abs name body, rest )

        OpenParen :: ts ->
            case parse_ast ts of
                ( a, Space :: a_ts ) ->
                    case parse_ast a_ts of
                        ( b, CloseParen :: b_ts ) ->
                            ( App a b, b_ts )

                        _ ->
                            ( Var "parse error in app", [] )

                _ ->
                    ( Var "parse error in app", [] )

        HoleToken :: ts ->
            ( Hole, ts )

        (ConstantToken c) :: ts ->
            ( Const c, ts )

        _ ->
            let
                _ =
                    Debug.log "parse error" tokens
            in
            ( Var "Parse error", [] )

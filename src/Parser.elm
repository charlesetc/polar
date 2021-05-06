module Parser exposing
    ( Result(..)
    , parse_many
    , parse_pattern
    , parse_single
    )

import Helpers exposing (..)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Pat
import Types exposing (..)


slurp_chars : List Token -> ( Ident, List Token, Int )
slurp_chars tokens =
    case tokens of
        (Char _ c) :: rest ->
            let
                ( v, ts, n ) =
                    slurp_chars rest
            in
            ( String.cons c v, ts, n + 1 )

        _ ->
            ( "", tokens, 0 )


parse_pattern : List Token -> ( Pat, List Token, Int )
parse_pattern tokens =
    case tokens of
        (HoleToken _) :: ts ->
            ( PatHole, ts, 1 )

        (Char _ _) :: _ ->
            let
                ( v, ts, n ) =
                    slurp_chars tokens
            in
            ( PatVar v, ts, n )

        _ ->
            ( PatVar "ParseError", tokens, 0 )


type Result a
    = Ok a
    | Error String


parse_sequence : List Token -> Result ( Sequence, List Token, Int )
parse_sequence tokens =
    case parse_single tokens of
        Ok ( a, (Op o) :: ts, n ) ->
            case parse_sequence ts of
                Ok ( seq, rest, m ) ->
                    Ok ( Cons a o seq, rest, n + 1 + m )

                Error e ->
                    Error e

        Ok ( a, CloseParen :: rest, n ) ->
            Ok ( End a, CloseParen :: rest, n )

        Ok ( a, [], n ) ->
            Ok ( End a, [], n )

        Ok ( _, _ :: rest, _ ) ->
            Error "error parsing sequence"

        Error e ->
            Error e


parse_many : List Token -> Result ( Ast, List Token, Int )
parse_many ts =
    case parse_sequence ts of
        Ok ( seq, rest, n ) ->
            Ok ( Sequence seq, rest, n )

        Error e ->
            Error e


parse_identifier : String -> Ast
parse_identifier s =
    if String.all Char.isDigit s then
        Const (Int (Maybe.withDefault 666 (String.toInt s)))

    else
        Var s


parse_single : List Token -> Result ( Ast, List Token, Int )
parse_single tokens =
    case tokens of
        (Char E _) :: _ ->
            let
                ( v, ts, n ) =
                    slurp_chars tokens
            in
            Ok ( parse_identifier v, ts, n )

        Lambda :: ts ->
            let
                ( p, ts2, n ) =
                    parse_pattern ts

                body =
                    case ts2 of
                        Dot :: ts3 ->
                            parse_single ts3

                        _ ->
                            Error "ParseError in lambda"
            in
            case body of
                Ok ( b, rest, m ) ->
                    Ok ( Abs p b, rest, n + m + 2 )

                Error e ->
                    Error e

        OpenParen :: ts ->
            case parse_many ts of
                Ok ( a, CloseParen :: a_ts, n ) ->
                    Ok ( a, a_ts, n + 2 )

                Ok _ ->
                    Error "parse error parsing sequence, expected ')'"

                Error s ->
                    Error s

        (HoleToken E) :: ts ->
            Ok ( Hole, ts, 1 )

        _ ->
            let
                _ =
                    Debug.log "parse error" tokens
            in
            Error "Parse error"

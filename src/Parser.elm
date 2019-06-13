module Parser exposing
    ( Result(..)
    , parse_many
    , parse_single
    )

import Helpers exposing (..)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Pat
import Types exposing (..)


slurp_chars : List Token -> ( Ident, List Token )
slurp_chars tokens =
    case tokens of
        (Char c) :: rest ->
            let
                ( v, ts ) =
                    slurp_chars rest
            in
            ( String.cons c v, ts )

        _ ->
            ( "", tokens )


parse_pattern : List Token -> ( Pat, List Token )
parse_pattern tokens =
    case tokens of
        HoleToken :: ts ->
            ( PatHole, ts )

        (Char _) :: _ ->
            let
                ( v, ts ) =
                    slurp_chars tokens
            in
            ( PatVar v, ts )

        _ ->
            ( PatVar "ParseError", tokens )


type Result a
    = Ok a
    | Error String


parse_sequence : List Token -> Result ( Sequence, List Token )
parse_sequence tokens =
    case parse_single tokens of
        Ok ( a, (Op o) :: ts ) ->
            case parse_sequence ts of
                Ok ( seq, rest ) ->
                    Ok ( Cons a o seq, rest )

                Error e ->
                    Error e

        Ok ( a, CloseParen :: rest ) ->
            Ok ( End a, CloseParen :: rest )

        Ok ( a, [] ) ->
            Ok ( End a, [] )

        Ok ( _, _ :: rest ) ->
            Error "error parsing sequence"

        Error e ->
            Error e


parse_many : List Token -> Result ( Ast, List Token )
parse_many ts =
    case parse_sequence ts of
        Ok ( seq, rest ) ->
            Ok ( Sequence seq, rest )

        Error e ->
            Error e


parse_identifier : String -> Ast
parse_identifier s =
    if String.all Char.isDigit s then
        Const (Int (Maybe.withDefault 666 (String.toInt s)))

    else
        Var s


parse_single : List Token -> Result ( Ast, List Token )
parse_single tokens =
    case tokens of
        (Char _) :: _ ->
            let
                ( v, ts ) =
                    slurp_chars tokens
            in
            Ok ( parse_identifier v, ts )

        Lambda :: ts ->
            let
                ( p, ts2 ) =
                    parse_pattern ts

                body =
                    case ts2 of
                        Dot :: ts3 ->
                            parse_single ts3

                        _ ->
                            Error "ParseError in lambda"
            in
            case body of
                Ok ( b, rest ) ->
                    Ok ( Abs p b, rest )

                Error e ->
                    Error e

        OpenParen :: ts ->
            case parse_many ts of
                Ok ( a, CloseParen :: a_ts ) ->
                    Ok ( a, a_ts )

                Ok _ ->
                    Error "parse error parsing sequence, expected ')'"

                Error s ->
                    Error s

        HoleToken :: ts ->
            Ok ( Hole, ts )

        _ ->
            let
                _ =
                    Debug.log "parse error" tokens
            in
            Error "Parse error"

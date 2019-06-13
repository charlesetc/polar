module Lambda exposing
    ( Lambda(..)
    , Result(..)
    , eval_ast
    , to_tokens
    , view_ast
    , view_error
    , view_lambda
    )

import Dict exposing (Dict)
import Helpers exposing (..)
import Html exposing (Html, div, text)
import Pat
import Token
import Types exposing (..)


type Lambda
    = Var Ident
    | Abs Pat Lambda
    | App Lambda Lambda
    | Hole
    | Const Constant


type alias Env =
    Dict Ident Lambda


view_ast : Ast -> Html Msg
view_ast ast =
    ast_to_lambda ast
        |> view_lambda


view_lambda : Lambda -> Html Msg
view_lambda l =
    to_tokens l |> Token.view_token_list


sequence_to_lambda : Sequence -> Lambda
sequence_to_lambda s =
    case s of
        End ast ->
            ast_to_lambda ast

        Cons a Space seq ->
            App (ast_to_lambda a) (sequence_to_lambda seq)


ast_to_lambda : Ast -> Lambda
ast_to_lambda a =
    case a of
        Types.Var i ->
            Var i

        Types.Abs pat body ->
            Abs pat (ast_to_lambda body)

        Types.Sequence sequence ->
            sequence_to_lambda sequence

        Types.Hole ->
            Hole

        Types.Const c ->
            Const c


type Error
    = UnboundVar String
    | NotAFunction Lambda
    | Impossible


view_error e =
    case e of
        UnboundVar s ->
            div [] [ text "unbound var ", text s ]

        NotAFunction a ->
            div [] [ text "not a function", view_lambda a ]

        Impossible ->
            text "oh boy, impossible"


type Step res
    = Step res
    | Value Lambda
    | StepError Error


type Result ast
    = Success ast
    | Error Error


chain :
    Env
    -> Lambda
    -> (Lambda -> Step ( Lambda, Env ))
    -> Step ( Lambda, Env )
chain env ast f =
    case step env ast of
        Step ( res, env2 ) ->
            chain env2 res f

        Value a ->
            f a

        StepError e ->
            StepError e


eval_ast : Ast -> Result Lambda
eval_ast ast =
    ast_to_lambda ast |> eval


eval : Lambda -> Result Lambda
eval ast =
    case chain Dict.empty ast (\x -> Value x) of
        Step _ ->
            Error Impossible

        Value a ->
            Success a

        StepError e ->
            Error e


step : Env -> Lambda -> Step ( Lambda, Env )
step env ast =
    case ast of
        Var name ->
            case Dict.get name env of
                Just binding ->
                    Step ( binding, env )

                Nothing ->
                    StepError (UnboundVar name)

        Abs _ _ ->
            Value ast

        Const _ ->
            Value ast

        Hole ->
            Value ast

        App a b ->
            chain env
                a
                (\a_ ->
                    chain env
                        b
                        (\b_ ->
                            case a_ of
                                Abs (PatVar var) body ->
                                    Step ( body, Dict.insert var b_ env )

                                Abs PatHole body ->
                                    Value ast

                                Hole ->
                                    -- we can't evaluate this function,
                                    -- so we'll treat it like a value
                                    Value ast

                                Var s ->
                                    StepError Impossible

                                -- this is not true for builtins
                                Const c ->
                                    StepError (NotAFunction (Const c))

                                App _ _ ->
                                    StepError Impossible
                        )
                )


to_tokens : Lambda -> List Token
to_tokens ast =
    case ast of
        App a b ->
            [ OpenParen ] ++ to_tokens a ++ [ Op Space ] ++ to_tokens b ++ [ CloseParen ]

        Abs p body ->
            [ Lambda ] ++ Pat.to_tokens p ++ [ Dot ] ++ to_tokens body

        Var v ->
            ident_to_tokens v

        Const (Str s) ->
            ident_to_tokens s

        Const (Int i) ->
            ident_to_tokens (String.fromInt i)

        Const (Builtin _) ->
            ident_to_tokens "BUILTIN"

        Hole ->
            [ HoleToken ]

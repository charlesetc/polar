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
import ShuntingYard exposing (..)
import Token
import Types exposing (..)


type Lambda
    = Var Ident
    | Abs Pat Lambda
    | App Lambda Lambda
    | BinaryOp Op Lambda Lambda
    | Hole
    | Const Constant


type alias Env =
    Dict Ident Lambda


view_ast : Ast -> Html Msg
view_ast ast =
    ast_to_lambda ast
        |> view_lambda


lambda_of_operation : Op -> Lambda -> Lambda -> Lambda
lambda_of_operation op a b =
    case op of
        Space ->
            App a b

        _ ->
            BinaryOp op a b


view_lambda : Lambda -> Html Msg
view_lambda l =
    to_tokens l |> Token.view_token_list


postfix_to_lambda : List Lambda -> ShuntingYard.SList -> Lambda
postfix_to_lambda stack slist =
    case slist of
        (A a) :: rest ->
            postfix_to_lambda (ast_to_lambda a :: stack) rest

        (O o) :: orest ->
            case stack of
                b :: a :: arest ->
                    let
                        lambda =
                            lambda_of_operation o a b
                    in
                    postfix_to_lambda (lambda :: arest) orest

                other ->
                    let
                        _ =
                            Debug.log "whoops -- malformed postfix 2" other
                    in
                    Var "error"

        [] ->
            case stack of
                [ l ] ->
                    l

                other ->
                    let
                        _ =
                            Debug.log "whoops -- malformed postfix 2" other
                    in
                    Var "error"


sequence_to_lambda : Sequence -> Lambda
sequence_to_lambda seq =
    let
        infix =
            ShuntingYard.slist_of_sequence seq

        postfix =
            ShuntingYard.shunting_yard [] infix
    in
    postfix_to_lambda [] postfix


ast_to_lambda : Ast -> Lambda
ast_to_lambda a =
    case a of
        Types.Var i ->
            Var i

        Types.Abs pat body ->
            Abs pat (ast_to_lambda body)

        Types.Let pat arg body ->
            App (Abs pat (ast_to_lambda body)) (ast_to_lambda arg)

        Types.Sequence sequence ->
            sequence_to_lambda sequence

        Types.Hole ->
            Hole

        Types.Const c ->
            Const c


type Error
    = UnboundVar String
    | NotAFunction Lambda
    | TypeError String
    | Impossible String


view_error e =
    case e of
        UnboundVar s ->
            div [] [ text "unbound var ", text s ]

        NotAFunction a ->
            div [] [ text "not a function ", view_lambda a ]

        Impossible s ->
            text ("oh boy, impossible " ++ s)

        TypeError s ->
            text ("type error: " ++ s)


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
    -> (Env -> Lambda -> Step ( Lambda, Env ))
    -> Step ( Lambda, Env )
chain env ast f =
    case step env ast of
        Step ( res, env2 ) ->
            chain env2 res f

        Value a ->
            f env a

        StepError e ->
            StepError e


eval_ast : Ast -> Result Lambda
eval_ast ast =
    ast_to_lambda ast |> eval


eval : Lambda -> Result Lambda
eval ast =
    case chain Dict.empty ast (\env x -> Value x) of
        Step _ ->
            Error (Impossible "can't get a step from a chain")

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

        BinaryOp o a b ->
            chain env
                a
                (\env2 a2 ->
                    chain env2
                        b
                        (\env3 b2 ->
                            case ( a2, b2 ) of
                                ( Const (Int a3), Const (Int b3) ) ->
                                    let
                                        fn =
                                            case o of
                                                Plus ->
                                                    (+)

                                                Minus ->
                                                    (-)

                                                Times ->
                                                    (*)

                                                Divide ->
                                                    \_ _ -> 2

                                                Space ->
                                                    Debug.log "why is there a space as a binary op?" (\_ _ -> 0)

                                        -- (/)
                                    in
                                    -- this env could be wrong
                                    Step ( Const (Int (fn a3 b3)), env )

                                _ ->
                                    --! fix this in the future.
                                    -- it should know if it's applied to an invalid value, like a lambda
                                    Value ast
                         -- StepError (TypeError "arguments to an op must be integers")
                        )
                )

        App a b ->
            chain env
                a
                (\env2 a2 ->
                    chain env2
                        b
                        (\env3 b2 ->
                            case a2 of
                                Abs (PatVar var) body ->
                                    Step ( body, Dict.insert var b2 env3 )

                                Abs PatHole body ->
                                    Value ast

                                Hole ->
                                    -- we can't evaluate this function,
                                    -- so we'll treat it like a value
                                    Value ast

                                Var s ->
                                    StepError (Impossible "got a var out of a chain")

                                -- must be a binary op with a hole
                                BinaryOp _ _ _ ->
                                    Value ast

                                Const c ->
                                    StepError (NotAFunction (Const c))

                                -- must be an app with a hole
                                App _ _ ->
                                    Value ast
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
            ident_to_tokens E v

        Const (Str s) ->
            ident_to_tokens E s

        Const (Int i) ->
            ident_to_tokens E (String.fromInt i)

        BinaryOp op a b ->
            [ OpenParen ] ++ to_tokens a ++ [ Op op ] ++ to_tokens b ++ [ CloseParen ]

        Hole ->
            [ HoleToken E ]

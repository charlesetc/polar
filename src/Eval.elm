module Eval exposing (Result(..), eval, view_error)

import Ast
import Dict
import Html
import Types exposing (..)


type Error
    = UnboundVar String
    | NotAFunction Ast
    | Impossible


view_error e =
    case e of
        UnboundVar s ->
            Html.div [] [ Html.text "unbound var", Html.text s ]

        NotAFunction a ->
            Html.div [] [ Html.text "not a function", Ast.view_ast a Nothing ]

        Impossible ->
            Html.text "oh boy, impossible"


type Step res
    = Step res
    | Value Ast
    | StepError Error


type Result ast
    = Success ast
    | Error Error


chain : Env -> Ast -> (Ast -> Step ( Ast, Env )) -> Step ( Ast, Env )
chain env ast f =
    case step env ast of
        Step ( res, env2 ) ->
            chain env2 res f

        Value a ->
            f a

        StepError e ->
            StepError e


eval : Ast -> Result Ast
eval ast =
    case chain Dict.empty ast (\x -> Value x) of
        Step _ ->
            Error Impossible

        Value a ->
            Success a

        StepError e ->
            Error e


step : Env -> Ast -> Step ( Ast, Env )
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
                                Abs var body ->
                                    Step ( body, Dict.insert var b_ env )

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

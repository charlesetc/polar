module Types exposing
    ( Ast(..)
    , Constant(..)
    , Cursor
    , Env
    , Ident
    , Model
    , Msg(..)
    , Token(..)
    )

import Dict exposing (Dict)
import Keyboard.Event exposing (KeyboardEvent)



-- Types for elm model


type alias Model =
    { tokens : List Token
    , cursor : Cursor
    }


type alias Cursor =
    Int


type Msg
    = Keyboard KeyboardEvent



-- Types for language interpretation


type alias Ident =
    String


type Constant
    = Str String
    | Int Int
    | Builtin (Ast -> Ast)


type Ast
    = Var Ident
    | Abs Ident Ast
    | App Ast Ast
    | Hole
    | Const Constant


type Token
    = Ident Ident
    | Lambda
    | OpenParen
    | CloseParen
    | Dot
    | Space
    | Cursor
    | HoleToken
    | ConstantToken Constant


type alias Env =
    Dict Ident Ast

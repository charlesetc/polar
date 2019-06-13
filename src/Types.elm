module Types exposing
    ( Ast(..)
    , Constant(..)
    , Ident
    , Model
    , Msg(..)
    , Op(..)
    , Pat(..)
    , Sequence(..)
    , Token(..)
    , ZToken
    )

import Keyboard.Event exposing (KeyboardEvent)



-- Types for elm model


type alias ZToken =
    ( List Token, Maybe Token, List Token )


type alias Model =
    ZToken


type Msg
    = Keyboard KeyboardEvent



-- Types for language interpretation


type alias Ident =
    String


type Constant
    = Str String
    | Int Int
    | Builtin (Ast -> Ast)


type Op
    = Space


type Sequence
    = Cons Ast Op Sequence
    | End Ast


type Ast
    = Var Ident
    | Abs Pat Ast
    | Sequence Sequence
    | Hole
    | Const Constant


type Pat
    = PatHole
    | PatVar Ident


type Token
    = Char Char
    | Lambda
    | OpenParen
    | CloseParen
    | Dot
    | Op Op
    | HoleToken

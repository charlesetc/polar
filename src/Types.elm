module Types exposing
    ( Ast(..)
    , BinaryOp(..)
    , Constant(..)
    , ContextType(..)
    , Ident
    , Model
    , Msg(..)
    , Op
    , Pat(..)
    , Sequence(..)
    , Token(..)
    , ZToken
    , string_of_op
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


type alias Op =
    BinaryOp


type BinaryOp
    = Space
    | Plus
    | Minus
    | Times
    | Divide


string_of_op : Op -> String
string_of_op o =
    case o of
        Space ->
            "<space>"

        Plus ->
            "+"

        Minus ->
            "-"

        Times ->
            "*"

        Divide ->
            "/"


type Sequence
    = Cons Ast Op Sequence
    | End Ast


type Ast
    = Var Ident
    | Abs Pat Ast
    | Let Pat Ast Ast
    | Sequence Sequence
    | Hole
    | Const Constant


type Pat
    = PatHole
    | PatVar Ident


type ContextType
    = P -- Pattern
    | E -- Expression


type Token
    = Char ContextType Char
    | Lambda
    | Equals
    | OpenParen
    | CloseParen
    | Dot
    | Op Op
    | HoleToken ContextType

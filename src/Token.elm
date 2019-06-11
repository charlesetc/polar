module Token exposing (view_token)

import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Types exposing (..)


view_token : Token -> Html Msg
view_token t =
    case t of
        Ident v ->
            text v

        Lambda ->
            text "λ"

        OpenParen ->
            text "("

        CloseParen ->
            text ")"

        Dot ->
            text "."

        Space ->
            text " "

        Cursor ->
            span [ class "cursor" ] []

        HoleToken ->
            span [ class "hole" ] []

        ConstantToken (Str s) ->
            span [ class "string" ] [ text s ]

        ConstantToken (Int i) ->
            span [ class "int" ] [ text (String.fromInt i) ]

        ConstantToken (Builtin _) ->
            span [ class "int" ] [ text "BUILTIN" ]

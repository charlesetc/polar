module Token exposing
    ( add_char
    , backspace
    , construct_lambda
    , move_cursor_left
    , move_cursor_right
    , next_hole
    , normalize_selection
    , open_paren
    , previous_hole
    , remove_cursor
    , space
    , view_token_list
    , view_ztoken
    )

import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Parser
import Types exposing (..)


move_cursor_right : ZToken -> ZToken
move_cursor_right z =
    case z of
        ( head, Just b, a :: tail ) ->
            -- we need this so that going right from a hole,
            -- across a space, into a hole, will select it.
            move_cursor_right ( b :: head, Nothing, a :: tail )

        ( head, Nothing, b :: tail ) ->
            ( b :: head, Nothing, tail )

        ( head, cursor, [] ) ->
            ( head, cursor, [] )


move_cursor_left : ZToken -> ZToken
move_cursor_left z =
    case z of
        ( a :: head, Just b, tail ) ->
            move_cursor_left ( a :: head, Nothing, b :: tail )

        ( b :: head, Nothing, tail ) ->
            ( head, Nothing, b :: tail )

        ( [], cursor, tail ) ->
            ( [], cursor, tail )


add_char : Char -> ZToken -> ZToken
add_char c ts =
    case ts of
        ( (Char i) :: head, Nothing, tail ) ->
            ( Char c :: Char i :: head, Nothing, tail )

        ( head, Just HoleToken, tail ) ->
            ( Char c :: head, Nothing, tail )

        ( head, Just a, tail ) ->
            ( head, Just a, tail )

        ( head, Nothing, tail ) ->
            ( head, Nothing, tail )


backspace : ZToken -> ZToken
backspace z =
    let
        delete_open_paren i tokens =
            case tokens of
                [] ->
                    []

                CloseParen :: rest ->
                    CloseParen :: delete_open_paren (i + 1) rest

                OpenParen :: rest ->
                    if i == 0 then
                        rest

                    else
                        OpenParen :: delete_open_paren (i - 1) rest

                a :: rest ->
                    a :: delete_open_paren i rest

        delete_closed_paren i tokens =
            case tokens of
                [] ->
                    []

                OpenParen :: rest ->
                    OpenParen :: delete_closed_paren (i + 1) rest

                CloseParen :: rest ->
                    if i == 0 then
                        rest

                    else
                        CloseParen :: delete_closed_paren (i - 1) rest

                a :: rest ->
                    a :: delete_closed_paren i rest
    in
    case z of
        ( (Char _) :: (Char a) :: head, Nothing, tail ) ->
            ( Char a :: head, Nothing, tail )

        ( (Char _) :: head, Nothing, (Char a) :: tail ) ->
            ( head, Nothing, Char a :: tail )

        ( (Char _) :: head, Nothing, tail ) ->
            ( head, Just HoleToken, tail )

        ( (Op Space) :: HoleToken :: head, cursor, tail ) ->
            ( head, cursor, tail )

        ( (Op Space) :: head, Just HoleToken, tail ) ->
            ( head, Nothing, tail )

        ( (Op Space) :: (Char a) :: head, Nothing, (Char b) :: tail ) ->
            ( Char a :: head, Nothing, Char b :: tail )

        ( (Op Space) :: head, Nothing, tail ) ->
            ( head, Nothing, Op Space :: tail )

        ( Dot :: head, Just HoleToken, tail ) ->
            ( head, Nothing, Dot :: HoleToken :: tail )

        ( Dot :: head, Nothing, tail ) ->
            ( head, Nothing, Dot :: tail )

        ( Lambda :: head, Just HoleToken, Dot :: tail ) ->
            ( head, Nothing, tail )

        ( Lambda :: head, Nothing, tail ) ->
            ( head, Nothing, Lambda :: tail )

        ( OpenParen :: head, cursor, tail ) ->
            ( head, cursor, delete_closed_paren 0 tail )

        ( CloseParen :: head, cursor, tail ) ->
            ( delete_open_paren 0 head, cursor, tail )

        ( head, cursor, tail ) ->
            ( head, cursor, tail )


space : ZToken -> ZToken
space z =
    case z of
        ( head, Just HoleToken, tail ) ->
            ( Op Space :: HoleToken :: head, Just HoleToken, tail )

        ( (Char a) :: head, Nothing, (Char b) :: tail ) ->
            ( Op Space :: Char a :: head, Nothing, tail )

        ( (Char a) :: head, Nothing, tail ) ->
            ( Op Space :: Char a :: head, Just HoleToken, tail )

        ( CloseParen :: head, Nothing, tail ) ->
            ( Op Space :: CloseParen :: head, Just HoleToken, tail )

        ( a, Nothing, tail ) ->
            ( a, Nothing, tail )

        ( head, Just c, tail ) ->
            ( head, Just c, tail )


open_paren : ZToken -> ZToken
open_paren z =
    case z of
        ( head, Just HoleToken, tail ) ->
            ( OpenParen :: head, Just HoleToken, CloseParen :: tail )

        -- ( head, Normal, tail) ->
        --    (AstwVk)
        ( head, cursor, tail ) ->
            ( head, cursor, tail )


next_hole : ZToken -> ZToken
next_hole original =
    let
        find_next_hole z =
            case z of
                ( head, Nothing, HoleToken :: tail ) ->
                    ( head, Just HoleToken, tail )

                ( head, Nothing, a :: tail ) ->
                    find_next_hole ( a :: head, Nothing, tail )

                ( _, Just _, _ ) ->
                    Debug.log "impossible" original

                ( _, Nothing, [] ) ->
                    original
    in
    case original of
        ( head, Just HoleToken, tail ) ->
            find_next_hole ( HoleToken :: head, Nothing, tail )

        _ ->
            find_next_hole original


previous_hole : ZToken -> ZToken
previous_hole original =
    let
        find_previous_hole z =
            case z of
                ( HoleToken :: head, Nothing, tail ) ->
                    ( head, Just HoleToken, tail )

                ( a :: head, Nothing, tail ) ->
                    find_previous_hole ( head, Nothing, a :: tail )

                ( _, Just _, _ ) ->
                    Debug.log "impossible" original

                ( [], Nothing, _ ) ->
                    original
    in
    case original of
        ( head, Just HoleToken, tail ) ->
            find_previous_hole ( head, Nothing, HoleToken :: tail )

        _ ->
            find_previous_hole original


construct_lambda : ZToken -> ZToken
construct_lambda z =
    case z of
        ( Lambda :: head, Just HoleToken, tail ) ->
            ( Lambda :: head, Just HoleToken, tail )

        ( head, Just HoleToken, tail ) ->
            ( Lambda :: head, Just HoleToken, Dot :: OpenParen :: HoleToken :: CloseParen :: tail )

        ( head, Nothing, Lambda :: tail ) ->
            ( Lambda :: head, Just HoleToken, Dot :: Lambda :: tail )

        --         ( head, Nothing, Dot :: tail ) ->
        --             ( Lambda :: Dot :: Hole :: head, Just HoleToken, Dot :: tail )
        ( head, cursor, tail ) ->
            ( head, cursor, tail )


remove_cursor : ZToken -> List Token
remove_cursor ( head, cursor, tail ) =
    case cursor of
        Nothing ->
            List.reverse head ++ tail

        Just c ->
            List.reverse head ++ [ c ] ++ tail



-- selects a hole if the cursor is next two it.
-- two holes next to each other never happens


normalize_selection : ZToken -> ZToken
normalize_selection z =
    case z of
        ( HoleToken :: head, Nothing, tail ) ->
            ( head, Just HoleToken, tail )

        ( head, Nothing, HoleToken :: tail ) ->
            ( head, Just HoleToken, tail )

        ( head, cursor, tail ) ->
            ( head, cursor, tail )



-- VIEW
-- maybe move these view functions to their own files


view_token : Token -> Html Msg
view_token t =
    case t of
        Char v ->
            text (String.fromChar v)

        Lambda ->
            text "λ"

        OpenParen ->
            text "("

        CloseParen ->
            text ")"

        Dot ->
            text "."

        Op Space ->
            text " "

        HoleToken ->
            span [ class "hole" ] [ text "□" ]


view_token_list tokens =
    span [ class "ast" ] (List.map view_token tokens)


view_ztoken : ZToken -> Html Msg
view_ztoken ( head, cursor, tail ) =
    let
        conv =
            List.map view_token

        head_txt =
            conv (List.reverse head)

        tail_txt =
            conv tail

        cursor_txt =
            [ case cursor of
                Nothing ->
                    span [ class "cursor" ] []

                Just HoleToken ->
                    span [ class "cursor", class "hole" ] [ text "□" ]

                Just a ->
                    span [ class "malformed-token" ] [ view_token a ]
            ]
    in
    span [ class "ast" ] (head_txt ++ cursor_txt ++ tail_txt)

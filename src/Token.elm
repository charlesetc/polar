module Token exposing
    ( add_char
    , backspace
    , construct_lambda
    , construct_op
    , construct_plus
    , move_cursor_left
    , move_cursor_right
    , move_paren_left
    , next_hole
    , normalize_selection
    , open_paren
    , previous_hole
    , remove_cursor
    , view_token_list
    , view_ztoken
    )

import Helpers exposing (..)
import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Parser
import Types exposing (..)


insert_at : Int -> Token -> List Token -> List Token
insert_at i token ts =
    case ( i, ts ) of
        ( 0, _ ) ->
            token :: ts

        ( _, t :: rest ) ->
            t :: insert_at (i - 1) token rest

        ( _, [] ) ->
            Debug.log "should not have gotten here"
                [ token ]


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
        ( head, Just (HoleToken ctx), tail ) ->
            ( Char ctx c :: head, Nothing, tail )

        ( (Char ctx i) :: head, Nothing, tail ) ->
            ( Char ctx c :: Char ctx i :: head, Nothing, tail )

        ( head, Nothing, OpenParen :: tail ) ->
            ( Char E c :: head, Nothing, Op Space :: OpenParen :: tail )

        ( head, Nothing, Lambda :: tail ) ->
            ( Char E c :: head, Nothing, Op Space :: Lambda :: tail )

        ( head, Nothing, (Char ctx a) :: tail ) ->
            ( Char ctx c :: head, Nothing, Char ctx a :: tail )

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
        ( (Char _ _) :: (Char ctx a) :: head, Nothing, tail ) ->
            ( Char ctx a :: head, Nothing, tail )

        ( (Char _ _) :: head, Nothing, (Char ctx a) :: tail ) ->
            ( head, Nothing, Char ctx a :: tail )

        ( (Char ctx _) :: head, Nothing, tail ) ->
            ( head, Just (HoleToken ctx), tail )

        ( (Op _) :: (HoleToken E) :: head, cursor, tail ) ->
            ( head, cursor, tail )

        ( (Op _) :: head, Just (HoleToken E), tail ) ->
            ( head, Nothing, tail )

        ( (Op _) :: (Char E a) :: head, Nothing, (Char E b) :: tail ) ->
            ( Char E a :: head, Nothing, Char E b :: tail )

        ( (Op o) :: head, Nothing, tail ) ->
            ( head, Nothing, Op o :: tail )

        ( Dot :: head, Just (HoleToken E), tail ) ->
            ( head, Nothing, Dot :: HoleToken E :: tail )

        ( Dot :: head, Nothing, tail ) ->
            ( head, Nothing, Dot :: tail )

        ( Lambda :: head, Just (HoleToken P), Dot :: tail ) ->
            ( head, Nothing, tail )

        ( Lambda :: head, Nothing, tail ) ->
            let
                ( _, ts, n ) =
                    Parser.parse_pattern tail
            in
            case ts of
                Dot :: rest ->
                    ( head, Nothing, rest )

                _ ->
                    Debug.log "uh-oh, invalid parse tree, needed a dot after a pattern" ( head, Nothing, tail )

        ( OpenParen :: head, cursor, tail ) ->
            ( head, cursor, delete_closed_paren 0 tail )

        ( CloseParen :: head, cursor, tail ) ->
            ( delete_open_paren 0 head, cursor, tail )

        ( [], Just (HoleToken E), (Op Space) :: tail ) ->
            ( [], Nothing, tail )

        ( head, cursor, tail ) ->
            ( head, cursor, tail )


construct_op : Op -> ZToken -> ZToken
construct_op op z =
    case z of
        ( head, Just (HoleToken E), tail ) ->
            ( Op op :: HoleToken E :: head, Just (HoleToken E), tail )

        ( (Char E a) :: head, Nothing, (Char E b) :: tail ) ->
            ( Op op :: Char E a :: head, Nothing, Char E b :: tail )

        ( (Char E a) :: head, Nothing, tail ) ->
            ( Op op :: Char E a :: head, Just (HoleToken E), tail )

        ( head, Nothing, (Char E a) :: tail ) ->
            ( head, Just (HoleToken E), Op op :: Char E a :: tail )

        ( CloseParen :: head, Nothing, tail ) ->
            ( Op op :: CloseParen :: head, Just (HoleToken E), tail )

        ( OpenParen :: head, Nothing, tail ) ->
            ( OpenParen :: head, Just (HoleToken E), Op op :: tail )

        ( head, Nothing, Dot :: tail ) ->
            if op == Space then
                ( Dot :: head, Nothing, tail )

            else
                ( head, Nothing, Dot :: tail )

        ( head, Nothing, OpenParen :: tail ) ->
            if op == Space then
                ( OpenParen :: head, Nothing, tail )

            else
                ( head, Nothing, OpenParen :: tail )

        ( a, Nothing, tail ) ->
            ( a, Nothing, tail )

        ( head, Just c, tail ) ->
            ( head, Just c, tail )


construct_eq : ZToken -> ZToken
construct_eq z =
    case z of
        ( OpenParen :: head, Nothing, tail ) ->
            ( OpenParen :: head, Just (HoleToken P), Equals :: tail )


move_paren_left : ZToken -> ZToken
move_paren_left z =
    let
        insert_paren tail =
            case Parser.parse_single tail of
                Parser.Ok ( _, _, n ) ->
                    insert_at n CloseParen tail

                Parser.Error e ->
                    Debug.log "oh no, a malformed token tree"
                        tail
    in
    case z of
        ( head, cursor, CloseParen :: (Op op) :: tail ) ->
            ( head, cursor, Op op :: insert_paren tail )

        ( head, cursor, tail ) ->
            ( head, cursor, tail )


open_paren : ZToken -> ZToken
open_paren z =
    let
        parenthesize head tail =
            case Parser.parse_single tail of
                Parser.Ok ( _, _, n ) ->
                    ( OpenParen :: head
                    , Nothing
                    , insert_at n CloseParen tail
                    )

                Parser.Error e ->
                    Debug.log "oh no, a malformed token tree"
                        ( head, Nothing, tail )
    in
    case z of
        ( head, Just (HoleToken E), tail ) ->
            ( OpenParen :: head, Just (HoleToken E), CloseParen :: tail )

        ( head, Nothing, OpenParen :: tail ) ->
            parenthesize head (OpenParen :: tail)

        ( head, Nothing, Lambda :: tail ) ->
            parenthesize head (Lambda :: tail)

        ( head, Nothing, (Char E c) :: tail ) ->
            parenthesize head (Char E c :: tail)

        -- ( head, Normal, tail) ->
        --    (AstwVk)
        ( head, cursor, tail ) ->
            ( head, cursor, tail )


next_hole : ZToken -> ZToken
next_hole original =
    let
        find_next_hole z =
            case z of
                ( head, Nothing, (HoleToken ctx) :: tail ) ->
                    ( head, Just (HoleToken ctx), tail )

                ( head, Nothing, a :: tail ) ->
                    find_next_hole ( a :: head, Nothing, tail )

                ( _, Just _, _ ) ->
                    Debug.log "impossible" original

                ( _, Nothing, [] ) ->
                    z
    in
    case original of
        ( head, Just (HoleToken ctx), tail ) ->
            find_next_hole ( HoleToken ctx :: head, Nothing, tail )

        _ ->
            find_next_hole original


previous_hole : ZToken -> ZToken
previous_hole original =
    let
        find_previous_hole z =
            case z of
                ( (HoleToken ctx) :: head, Nothing, tail ) ->
                    ( head, Just (HoleToken ctx), tail )

                ( a :: head, Nothing, tail ) ->
                    find_previous_hole ( head, Nothing, a :: tail )

                ( _, Just _, _ ) ->
                    Debug.log "impossible" original

                ( [], Nothing, _ ) ->
                    z
    in
    case original of
        ( head, Just (HoleToken ctx), tail ) ->
            find_previous_hole ( head, Nothing, HoleToken ctx :: tail )

        _ ->
            find_previous_hole original


construct_plus : ZToken -> ZToken
construct_plus z =
    case z of
        ( head, Just (HoleToken E), tail ) ->
            ( Op Plus :: HoleToken E :: head, Just (HoleToken E), tail )

        ( head, cursor, tail ) ->
            ( head, cursor, tail )


construct_lambda : ZToken -> ZToken
construct_lambda z =
    case z of
        ( Lambda :: head, Just (HoleToken P), Dot :: tail ) ->
            ( Lambda :: Dot :: HoleToken P :: Lambda :: head, Just (HoleToken P), Dot :: tail )

        ( Lambda :: head, Just (HoleToken P), tail ) ->
            ( Lambda :: head, Just (HoleToken P), tail )

        ( head, Nothing, Dot :: tail ) ->
            ( Lambda :: Dot :: head, Just (HoleToken P), Dot :: tail )

        ( head, Just (HoleToken E), tail ) ->
            ( Lambda :: head, Just (HoleToken P), Dot :: OpenParen :: HoleToken E :: CloseParen :: tail )

        ( head, Nothing, Lambda :: tail ) ->
            ( Lambda :: head, Just (HoleToken P), Dot :: Lambda :: tail )

        ( head, Nothing, OpenParen :: tail ) ->
            ( Lambda :: head, Just (HoleToken P), Dot :: OpenParen :: tail )

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
        ( (HoleToken ctx) :: head, Nothing, tail ) ->
            ( head, Just (HoleToken ctx), tail )

        ( head, Nothing, (HoleToken ctx) :: tail ) ->
            ( head, Just (HoleToken ctx), tail )

        ( head, cursor, tail ) ->
            ( head, cursor, tail )



-- VIEW
-- maybe move these view functions to their own files


view_token : Token -> Html Msg
view_token t =
    case t of
        Char _ v ->
            text (String.fromChar v)

        Lambda ->
            text "λ"

        Equals ->
            text "="

        OpenParen ->
            text "("

        CloseParen ->
            text ")"

        Dot ->
            text "."

        Op Space ->
            text " "

        Op o ->
            span [ class "op" ] [ text (string_of_op o) ]

        HoleToken _ ->
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

                Just (HoleToken _) ->
                    span [ class "cursor", class "hole" ] [ text "□" ]

                Just a ->
                    span [ class "malformed-token" ] [ view_token a ]
            ]
    in
    span [ class "ast" ] (head_txt ++ cursor_txt ++ tail_txt)

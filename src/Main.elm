module Main exposing (init, main, subscriptions, update, view)

-- import Normalize

import Array
import Browser
import Html exposing (Html)
import Html.Attributes exposing (autofocus, class, tabindex)
import Html.Events exposing (preventDefaultOn)
import Json.Decode
import Keyboard.Event exposing (decodeKeyboardEvent)
import Lambda
import Parser
import Token
import Types exposing (..)



-- LOGIC


init : () -> ( Model, Cmd Msg )
init _ =
    ( ( [], Just (HoleToken E), [] )
    , Cmd.none
    )



-- VIEW


should_prevent_default a =
    case a of
        Keyboard { key } ->
            key == Just "Tab" || key == Just "/"


view : Model -> Html Msg
view model =
    let
        ast_view =
            Html.div [ class "result" ]
                [ case Token.remove_cursor model |> Parser.parse_many of
                    Parser.Ok ( ast, [], _ ) ->
                        Html.span []
                            [ Html.div [ class "parsed" ]
                                [ Lambda.view_ast ast
                                ]
                            , case Lambda.eval_ast ast of
                                Lambda.Success a ->
                                    Html.div [ class "evaluated" ]
                                        [ Lambda.view_lambda a ]

                                Lambda.Error e ->
                                    Lambda.view_error e
                            ]

                    Parser.Ok ( ast, rest, _ ) ->
                        Html.div
                            [ class "parse-error" ]
                            [ Lambda.view_ast ast
                            , Html.text "got extra tokens:"
                            , Token.view_token_list rest
                            ]

                    Parser.Error e ->
                        Html.div
                            [ class "parse-error" ]
                            [ Html.text ("Ast error: " ++ e) ]
                ]
    in
    Html.div
        [ class "main-view"
        , preventDefaultOn "keydown" <|
            (Json.Decode.map Keyboard decodeKeyboardEvent |> Json.Decode.map (\x -> ( x, should_prevent_default x )))
        , tabindex 0
        , autofocus True
        ]
        [ Token.view_ztoken model
        , ast_view
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "received event" msg

        new_model =
            case msg of
                Keyboard { key, shiftKey } ->
                    case key of
                        Just "ArrowLeft" ->
                            Token.move_cursor_left model

                        Just "ArrowRight" ->
                            Token.move_cursor_right model

                        Just "Backspace" ->
                            Token.backspace model

                        Just " " ->
                            Token.construct_op Space model

                        Just "(" ->
                            Token.open_paren model

                        Just "}" ->
                            Token.move_paren_left model

                        Just "+" ->
                            Token.construct_op Plus model

                        Just "-" ->
                            Token.construct_op Minus model

                        Just "*" ->
                            Token.construct_op Times model

                        Just "/" ->
                            Token.construct_op Divide model

                        Just "=" ->
                            Token.construct_eq model

                        Just "Tab" ->
                            if shiftKey then
                                Token.previous_hole model

                            else
                                Token.next_hole model

                        Just "\\" ->
                            Token.construct_lambda model

                        Just s ->
                            case String.uncons s of
                                Just ( c, "" ) ->
                                    if Char.isAlphaNum c then
                                        Token.add_char c model

                                    else
                                        model

                                _ ->
                                    model

                        _ ->
                            model
    in
    ( Token.normalize_selection new_model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

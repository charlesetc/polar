module Main exposing (init, main, subscriptions, update, view)

import Array
import Ast
import Browser
import Eval
import Html exposing (Html)
import Html.Attributes exposing (autofocus, class, tabindex)
import Html.Events exposing (on)
import Json.Decode
import Keyboard.Event exposing (decodeKeyboardEvent)
import Types exposing (..)



-- LOGIC


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ast =
            App (Abs "x" (App (Var "x") Hole)) (Abs "x" (Const (Int 0)))
    in
    ( { tokens = Ast.to_tokens ast
      , cursor = 2
      }
    , Cmd.none
    )



-- VIEW


view : Model -> Html Msg
view model =
    let
        ( ast, _ ) =
            Ast.parse_ast model.tokens
    in
    Html.div
        [ class "main-view"
        , on "keydown" <|
            Json.Decode.map Keyboard decodeKeyboardEvent
        , tabindex 0
        , autofocus True
        ]
        [ Ast.view_ast ast (Just model.cursor)
        , Html.div [ class "result" ]
            [ case Eval.eval ast of
                Eval.Success a ->
                    Ast.view_ast a Nothing

                Eval.Error e ->
                    Eval.view_error e
            ]
        ]



-- UPDATE


type Action
    = Left
    | Right


update_action : Maybe Action -> Model -> Model
update_action a model =
    case a of
        Just Left ->
            if model.cursor > 0 then
                let
                    token_array =
                        Array.fromList model.tokens
                in
                { model | cursor = model.cursor - 1 }

            else
                model

        Just Right ->
            if model.cursor < (model.tokens |> List.length) then
                { model | cursor = model.cursor + 1 }

            else
                model

        Nothing ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "received event" msg

        action =
            case msg of
                Keyboard { key } ->
                    case key of
                        Just "ArrowLeft" ->
                            Just Left

                        Just "ArrowRight" ->
                            Just Right

                        Just "l" ->
                            Just Right

                        Just "h" ->
                            Just Left

                        _ ->
                            Nothing

        new_model =
            update_action action model
    in
    ( new_model, Cmd.none )



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

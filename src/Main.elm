module Main exposing (main)

import Browser
import Caesar exposing (crack, encode)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    String


init : Model
init =
    ""



-- UPDATE


type Msg
    = Encode Int String
    | Crack String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Encode shift plaintext ->
            Caesar.encode shift plaintext

        Crack ciphertext ->
            Caesar.crack ciphertext



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Caesar cipher" ]
        , text "Encode"
        , select []
            [ option [] [ text <| String.fromInt 1 ]
            , option [] [ text <| String.fromInt 2 ]
            , option [] [ text <| String.fromInt 3 ]
            , option [] [ text <| String.fromInt 4 ]
            , option [] [ text <| String.fromInt 5 ]
            , option [] [ text <| String.fromInt 6 ]
            , option [] [ text <| String.fromInt 7 ]
            , option [] [ text <| String.fromInt 8 ]
            , option [] [ text <| String.fromInt 9 ]
            , option [] [ text <| String.fromInt 0 ]
            , option [] [ text <| String.fromInt 11 ]
            , option [] [ text <| String.fromInt 12 ]
            , option [] [ text <| String.fromInt 13 ]
            , option [] [ text <| String.fromInt 14 ]
            , option [] [ text <| String.fromInt 15 ]
            , option [] [ text <| String.fromInt 16 ]
            , option [] [ text <| String.fromInt 17 ]
            , option [] [ text <| String.fromInt 18 ]
            , option [] [ text <| String.fromInt 19 ]
            , option [] [ text <| String.fromInt 20 ]
            , option [] [ text <| String.fromInt 21 ]
            , option [] [ text <| String.fromInt 22 ]
            , option [] [ text <| String.fromInt 23 ]
            , option [] [ text <| String.fromInt 24 ]
            , option [] [ text <| String.fromInt 25 ]
            , option [] [ text <| String.fromInt 26 ]
            ]
        , input [ placeholder "plaintext" ] []
        , button [ onClick (Encode 0 "") ] [ text "Encode" ]
        , div [] []
        , text "Crack"
        , input [ placeholder "ciphertext" ] []
        , button [ onClick (Crack "") ] [ text "Crack" ]
        , div [] [ text model ]
        ]

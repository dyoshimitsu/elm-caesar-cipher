module Main exposing (main)

import Browser
import Caesar exposing (crack, encode)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Json.Decode


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { encode :
        { shift : Int
        , plaintext : String
        , ciphertext : String
        }
    , crack :
        { ciphertext : String
        , decryption : String
        }
    }


init : Model
init =
    { encode =
        { shift = 0
        , plaintext = ""
        , ciphertext = ""
        }
    , crack =
        { ciphertext = ""
        , decryption = ""
        }
    }



-- UPDATE


type Msg
    = ShiftChange String
    | PlaintextChange String
    | Encode Int String
    | CiphertextChange String
    | Crack String


onChange : (String -> msg) -> Attribute msg
onChange handler =
    on "change" (Json.Decode.map handler Html.Events.targetValue)


update : Msg -> Model -> Model
update msg model =
    case msg of
        ShiftChange shift ->
            { model
                | encode =
                    { shift = Maybe.withDefault 0 (String.toInt shift)
                    , plaintext = model.encode.plaintext
                    , ciphertext = model.encode.ciphertext
                    }
            }

        PlaintextChange plaintext ->
            { model
                | encode =
                    { shift = model.encode.shift
                    , plaintext = plaintext
                    , ciphertext = model.encode.ciphertext
                    }
            }

        Encode shift plaintext ->
            { model
                | encode =
                    { shift = shift
                    , plaintext = plaintext
                    , ciphertext = Caesar.encode shift plaintext
                    }
            }

        CiphertextChange ciphertext ->
            { model
                | crack =
                    { ciphertext = ciphertext
                    , decryption = model.crack.decryption
                    }
            }

        Crack ciphertext ->
            { model
                | crack =
                    { ciphertext = model.crack.ciphertext
                    , decryption = Caesar.crack ciphertext
                    }
            }



-- VIEW


view : Model -> Html Msg
view model =
    let
        handler selectedValue =
            ShiftChange selectedValue
    in
    div []
        [ h1 [] [ text "Caesar cipher" ]
        , h2 [] [ text "Encode" ]
        , select [ onChange handler ]
            (List.range 1 26 |> List.map (\n -> option [ value (String.fromInt n) ] [ text (String.fromInt n) ]))
        , input [ placeholder "plaintext", value model.encode.plaintext, onInput PlaintextChange ] []
        , button [ onClick (Encode model.encode.shift model.encode.plaintext) ] [ text "Encode" ]
        , div [] [ text model.encode.ciphertext ]
        , h2 [] [ text "Crack" ]
        , input [ placeholder "ciphertext", value model.crack.ciphertext, onInput CiphertextChange ] []
        , button [ onClick (Crack model.crack.ciphertext) ] [ text "Crack" ]
        , div [] [ text model.crack.decryption ]
        ]

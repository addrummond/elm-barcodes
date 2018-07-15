module Barcode.Input exposing (Config, Msgs, State, clear, defaultConfig, enter, init, isEmpty, reset, update, view)

{-| A view-only widget for receiving input from barcode scanners. It consists
of a text input and an optional "entered value" displayed to the right of the
input. A String -> Bool predicate determines whether or not the contents
of the text input and/or the entered value are displayed with an error
highlight. The input state is exposed via the opaque 'BarcodeInputValue' record,
an instance of which should live in the parent state.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Json.Decode as Json
import Maybe
import Maybe.Extra as Maybe
import SemanticUI as SemanticUI
import SemanticUI.Elements.Label as Label


type alias Config msg =
    { isError : String -> Bool
    , size : Int
    , attributes : List (Attribute msg)
    }


type alias Msgs msg =
    { onInput : String -> msg
    , onEnter : String -> msg
    , noOp : msg
    }


type State
    = State
        { clearKey : Int
        , transientValue : String
        , enteredValue : Maybe String
        }


{-| The initial value for the text input (empty string).
-}
init : State
init =
    State
        { clearKey = 0
        , transientValue = ""
        , enteredValue = Nothing
        }


{-| Reset the text input to its initial state.
-}
reset : State -> State
reset (State biv) =
    -- From an external point of view, this returns the barcode's text input to
    -- its initial state. Internally, we need to increment 'clearKey'.
    State
        { clearKey = biv.clearKey + 1
        , transientValue = ""
        , enteredValue = Nothing
        }


{-| Empy the text input.
-}
clear : State -> State
clear (State biv) =
    State
        { clearKey = biv.clearKey + 1
        , transientValue = ""
        , enteredValue = biv.enteredValue
        }


{-| Modify the value of the text input. If the second argument is the empty
string, this is equivalent to 'clear'.
-}
update : State -> String -> State
update (State biv) v =
    case v of
        "" ->
            clear (State biv)

        _ ->
            State
                { clearKey = biv.clearKey
                , transientValue = v
                , enteredValue = biv.enteredValue
                }


{-| Add an "entered value" to the right of the text input and clear the text
input.
-}
enter : State -> String -> State
enter (State biv) v =
    State
        { clearKey = biv.clearKey + 1
        , transientValue = ""
        , enteredValue =
            case v of
                "" ->
                    Nothing

                _ ->
                    Just v
        }


{-| Returns true iff nothing has been entered in the input.
-}
isEmpty : State -> Bool
isEmpty (State biv) =
    biv.transientValue == "" && biv.enteredValue == Nothing


defaultConfig : Config msg
defaultConfig =
    { isError = always False
    , size = 30
    , attributes = []
    }


onKeyDownWithValue : (Int -> String -> msg) -> Attribute msg
onKeyDownWithValue tagger =
    on "keydown" <|
        Json.map2
            tagger
            keyCode
            (Json.field "target" <| Json.field "value" Json.string)


view : Config msg -> Msgs msg -> State -> Html msg
view { isError, size, attributes } { onInput, onEnter, noOp } (State { clearKey, transientValue, enteredValue }) =
    let
        lab =
            Label.init
    in
    div []
        [ Keyed.node "div"
            -- elm-semantic-ui doesn't provide 'labeled' class
            [ class "ui input"
            , classList
                [ ( "right", Maybe.isJust enteredValue )
                , ( "labeled", Maybe.isJust enteredValue )
                , ( "error", isError transientValue )
                ]
            ]
          <|
            List.concat
                [ [ ( toString clearKey
                    , input
                        ([ type_ "text"
                         , Html.Attributes.size size
                         , Html.Events.onInput onInput
                         , onKeyDownWithValue
                            (\key value ->
                                case key of
                                    13 ->
                                        onEnter value

                                    _ ->
                                        noOp
                            )
                         , defaultValue ""
                         ]
                            ++ attributes
                        )
                        []
                    )
                  ]
                , case enteredValue of
                    Nothing ->
                        []

                    Just v ->
                        [ ( ""
                          , Label.label
                                { lab
                                    | color =
                                        case enteredValue of
                                            Nothing ->
                                                Nothing

                                            Just v ->
                                                if isError v then
                                                    Just SemanticUI.Red
                                                else
                                                    Nothing
                                }
                                v
                          )
                        ]
                ]
        ]

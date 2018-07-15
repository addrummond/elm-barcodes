module Barcodes.Input exposing (Config, Msgs, State, clear, defaultConfig, enter, enteredValue, init, isEmpty, reset, update, view)

{-| A view-only component for receiving input from barcode scanners. It consists
of a text input and an optional "entered value" displayed to the right of the
input. A String -> Bool predicate determines whether or not the contents
of the text input and/or the entered value are displayed with an error
highlight. The input state is exposed via the opaque 'BarcodeInputValue' record,
an instance of which should live in the parent state.


# Component configuration, messages and state

@docs Config, Msgs, State


# Initialize, update and interrogate component state

@docs init, defaultConfig, clear, enter, enteredValue, isEmpty, reset, update


# View the component

@docs view

-}

--import Maybe.Extra as Maybe

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Json.Decode as Json
import Maybe


{-| The size and list of attributes for the text <input>.
-}
type alias Config msg =
    { size : Int
    , attributes : List (Attribute msg)
    }


{-| Specifies a no-op message and messages triggered on input and when enter
is pressed.
-}
type alias Msgs msg =
    { onInput : String -> msg
    , onEnter : String -> msg
    , noOp : msg
    }


{-| This should live somewhere in the state of the parent component.
-}
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


{-| Empty the text input.
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


{-| The value, if any, which has been scanned in.
-}
enteredValue : State -> Maybe String
enteredValue (State { enteredValue }) =
    enteredValue


{-| Returns true iff nothing has been entered in the input.
-}
isEmpty : State -> Bool
isEmpty (State biv) =
    biv.transientValue == "" && biv.enteredValue == Nothing


{-| Default configuration
-}
defaultConfig : Config msg
defaultConfig =
    { size = 30
    , attributes = []
    }


onKeyDownWithValue : (Int -> String -> msg) -> Attribute msg
onKeyDownWithValue tagger =
    on "keydown" <|
        Json.map2
            tagger
            keyCode
            (Json.field "target" <| Json.field "value" Json.string)


{-| View the component.
-}
view : Config msg -> Msgs msg -> State -> Html msg
view { size, attributes } { onInput, onEnter, noOp } (State { clearKey, transientValue, enteredValue }) =
    div []
        [ Keyed.node "input" [] <|
            [ ( toString clearKey
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
        ]

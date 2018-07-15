module Barcodes.Input exposing (Config, Msgs, State, attributes, clear, defaultConfig, enter, init, isEmpty, reset, update, view)

{-| A view-only component for receiving input from barcode scanners. The input
state is exposed via the opaque 'BarcodeInputValue' record, an instance of which
should live in the parent state.

# Component configuration and state

@docs Config, Msgs, State

# Manage component state

@docs init, defaultConfig, attributes, clear, enter, isEmpty, reset, update

# View

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
    , triggerKeyCodes : List Int
    }


{-| Specifies a no-op message and the messages triggered on input and when enter
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
        -- Currently, transientValue cannot be accessed via this modules API
        -- except via isEmpty. We could allow transientValue to be read, but
        -- this would then make it easier to write incorrect code.
        , transientValue : String
        }


{-| The state of an empty barcode input.
-}
init : State
init =
    State
        { clearKey = 0
        , transientValue = ""
        }


{-| Reset the barcode input to its initial state.
-}
reset : State -> State
reset (State biv) =
    -- From an external point of view, this returns the barcode's text input to
    -- its initial state. Internally, we need to increment 'clearKey'.
    State
        { clearKey = biv.clearKey + 1
        , transientValue = ""
        }


{-| Empty the barcode input.
-}
clear : State -> State
clear (State biv) =
    State
        { clearKey = biv.clearKey + 1
        , transientValue = ""
        }


{-| Modify the value of the barcode input. If the second argument is the empty
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
                }


{-| Set the entered value of the barcode input and then clear the text input
box.
-}
enter : State -> String -> State
enter (State biv) v =
    State
        { clearKey = biv.clearKey + 1
        , transientValue = ""
        }


{-| Returns true iff nothing has been entered in the input. This may give an
incorrect result if called very shorly after text has been entered into the
input field.
-}
isEmpty : State -> Bool
isEmpty (State biv) =
    biv.transientValue == ""


{-| Default configuration
-}
defaultConfig : Config msg
defaultConfig =
    { size = 30
    , attributes = []
    , triggerKeyCodes = [13]
    }


{-| Set the attributes filed of Config
-}
attributes : List (Attribute msg) -> Config msg -> Config msg
attributes atrs conf =
    { conf | attributes = atrs }


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
view { size, attributes, triggerKeyCodes } { onInput, onEnter, noOp } (State { clearKey }) =
    div []
        [ Keyed.node "div" [] <|
            [ ( toString clearKey
              , input
                    ([ type_ "text"
                     , Html.Attributes.size size
                     , Html.Events.onInput onInput
                     , onKeyDownWithValue
                        (\key value ->
                            if List.member key triggerKeyCodes then
                                onEnter value
                            else
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

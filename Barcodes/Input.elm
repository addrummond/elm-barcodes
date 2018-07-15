module Barcodes.Input exposing (Config, Msgs, State, allowEmptyScans, attributes, clear, clobber, defaultConfig, enter, init, isEmpty, reset, update, view)

{-| A view-only component for receiving input from barcode scanners. The input
state is exposed via the opaque BarcodeInputValue record, an instance of which
should live in the parent state.


# Component configuration and state

@docs Config, Msgs, State


# Manage component state

@docs init, defaultConfig, attributes, allowEmptyScans, clear, clobber, enter, isEmpty, reset, update


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


{-| The size and list of attributes for the text input node
-}
type alias Config msg =
    { size : Int
    , attributes : List (Attribute msg)
    , triggerKeyCodes : List Int
    , allowEmptyScans : Bool
    }


{-| Specifies a no-op message, the message triggered on text input, and the
message triggered when the scan terminates
-}
type alias Msgs msg =
    { onInput : String -> msg
    , onEnter : String -> msg
    , noOp : msg
    }


{-| This should live somewhere in the state of the parent component
-}
type State
    = State
        { clearKey : Int

        -- Currently, transientValue cannot be accessed via this modules API
        -- except via isEmpty. We could allow transientValue to be read, but
        -- this would then make it easier to write incorrect code.
        , transientValue : String
        , initialValue : String
        }


{-| The state of an empty barcode input
-}
init : State
init =
    State
        { clearKey = 0
        , transientValue = ""
        , initialValue = ""
        }


{-| Reset the barcode input to its initial state
-}
reset : State -> State
reset (State biv) =
    -- From an external point of view, this returns the barcode's text input to
    -- its initial state. Internally, we need to increment 'clearKey'.
    State
        { clearKey = biv.clearKey + 1
        , transientValue = ""
        , initialValue = ""
        }


{-| Set the text in the input field to the given value. As the name suggests,
this not something you would typically want to do. In particular, you should NOT
typically call this function to update the state in response to OnInput
messages.
-}
clobber : String -> State -> State
clobber v (State biv) =
    State
        { clearKey = biv.clearKey + 1
        , transientValue = v
        , initialValue = v
        }


{-| Empty the barcode input
-}
clear : State -> State
clear s =
    clobber "" s


{-| This function should be used to update the state in response to OnUpdate
messages.
-}
update : String -> State -> State
update v (State biv) =
    State
        { clearKey = biv.clearKey
        , transientValue = v
        , initialValue = biv.initialValue
        }


{-| This function should be used to update the state in response to OnEnter
messages.
-}
enter : State -> State
enter s =
    clear s


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
    , triggerKeyCodes = [ 13 ]
    , allowEmptyScans = False
    }


{-| Set the attributes filed of Config
-}
attributes : List (Attribute msg) -> Config msg -> Config msg
attributes atrs conf =
    { conf | attributes = atrs }


{-| Set the allowEmptyScans field of Config
-}
allowEmptyScans : Bool -> Config msg -> Config msg
allowEmptyScans b conf =
    { conf | allowEmptyScans = b }


onKeyDownWithValue : (Int -> String -> msg) -> Attribute msg
onKeyDownWithValue tagger =
    on "keydown" <|
        Json.map2
            tagger
            keyCode
            (Json.field "target" <| Json.field "value" Json.string)


{-| View the component
-}
view : Config msg -> Msgs msg -> State -> Html msg
view { size, attributes, triggerKeyCodes, allowEmptyScans } { onInput, onEnter, noOp } (State { clearKey, initialValue }) =
    div []
        [ Keyed.node "div" [] <|
            [ ( toString clearKey
              , input
                    ([ type_ "text"
                     , Html.Attributes.size size
                     , Html.Events.onInput onInput
                     , onKeyDownWithValue
                        (\key value ->
                            if (allowEmptyScans || String.length value > 0) && List.member key triggerKeyCodes then
                                onEnter value
                            else
                                noOp
                        )
                     , defaultValue initialValue
                     ]
                        ++ attributes
                    )
                    []
              )
            ]
        ]

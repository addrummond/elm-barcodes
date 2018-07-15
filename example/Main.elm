module Main exposing (..)

import Barcodes.Input as BI
import Barcodes.Render as BR
import Dom
import Html exposing (Html, div, h2, input, li, program, span, text, ul)
import Html.Attributes exposing (class, id, style)
import Task


type alias Model =
    { inputState : BI.State
    , scans : List String
    }


type Msg
    = NoOp
    | OnInput String
    | OnEnter String


init : ( Model, Cmd Msg )
init =
    ( { inputState = BI.init
      , scans = []
      }
    , Dom.focus "barcodeinput" |> Task.attempt (always NoOp)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OnInput s ->
            ( { model | inputState = BI.update model.inputState s }
            , Cmd.none
            )

        OnEnter s ->
            ( { model
                | inputState = BI.clear model.inputState
                , scans = s :: model.scans
              }
            , Dom.focus "barcodeinput" |> Task.attempt (always NoOp)
            )


view : Model -> Html Msg
view { inputState, scans } =
    div [ style [ ( "width", "700px" ), ( "margin", "auto" ) ] ]
        [ h2 [] [ text "Example code 39 barcode render" ]
        , case BR.code39 (BR.init |> BR.barcode "ABCD1234XYZ987") of
            Just b ->
                b

            Nothing ->
                text "Bad barcode"
        , h2 [ style [ ( "margin-top", "2em" ) ] ] [ text "Scan barcodes into the input" ]
        , BI.view (BI.defaultConfig |> BI.attributes [ id "barcodeinput", style [ ( "font-size", "x-large" ) ] ])
            { noOp = NoOp, onInput = OnInput, onEnter = OnEnter }
            inputState
        , div [ style [ ( "margin-top", "2em" ) ] ]
            [ text "Scans:"
            , ul
                []
                (scans
                    |> List.map
                        (\scan ->
                            li [] [ text scan ]
                        )
                )
            ]
        ]


main : Program Never Model Msg
main =
    program { init = init, view = view, update = update, subscriptions = always Sub.none }

module Barcodes.Render exposing (Barcode, barHeight, barWidth, barcode, code39BarcodeSvg, init, svgAttributes, wrapperDivAttributes)

{-| A module for rendering barcodes using inline SVG. Currently, only code 39
barcodes are supported.

@docs Barcode, barHeight, barWidth, barcode, code39BarcodeSvg, init, svgAttributes, wrapperDivAttributes

-}

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import List
import Maybe
import State exposing (State(..))
import Svg
import Svg.Attributes


type Bar
    = WideBar
    | NarrowBar
    | WideSpace


parseBars : String -> List Bar
parseBars =
    String.toList
        >> List.map
            (\c ->
                case c of
                    ':' ->
                        WideBar

                    '|' ->
                        NarrowBar

                    _ ->
                        WideSpace
            )


code39 : Dict Char (List Bar)
code39 =
    Dict.fromList <|
        List.map (\( c, s ) -> ( c, parseBars s ))
            [ ( 'A', ":|| |:" )
            , ( 'B', "|:| |:" )
            , ( 'C', "::| ||" )
            , ( 'D', "||: |:" )
            , ( 'E', ":|: ||" )
            , ( 'F', "|:: ||" )
            , ( 'G', "||| ::" )
            , ( 'H', ":|| :|" )
            , ( 'I', "|:| :|" )
            , ( 'J', "||: :|" )
            , ( 'K', ":||| :" )
            , ( 'L', "|:|| :" )
            , ( 'M', "::|| |" )
            , ( 'N', "||:| :" )
            , ( 'O', ":|:| |" )
            , ( 'P', "|::| |" )
            , ( 'Q', "|||: :" )
            , ( 'R', ":||: |" )
            , ( 'S', "|:|: |" )
            , ( 'T', "||:: |" )
            , ( 'U', ": |||:" )
            , ( 'V', "| :||:" )
            , ( 'W', ": :|||" )
            , ( 'X', "| |:|:" )
            , ( 'Y', ": |:||" )
            , ( 'Z', "| ::||" )
            , ( '0', "|| ::|" )
            , ( '1', ":| ||:" )
            , ( '2', "|: ||:" )
            , ( '3', ":: |||" )
            , ( '4', "|| :|:" )
            , ( '5', ":| :||" )
            , ( '6', "|: :||" )
            , ( '7', "|| |::" )
            , ( '8', ":| |:|" )
            , ( '9', "|: |:|" )
            , ( ' ', "| :|:|" )
            , ( '-', "| ||::" )
            , ( '$', "| | | ||" )
            , ( '%', "|| | | |" )
            , ( '.', ": ||:|" )
            , ( '/', "| | || |" )
            , ( '+', "| || | |" )
            ]


code39StartStop : List Bar
code39StartStop =
    parseBars "| |::|"


widthOfBar : Bar -> Int
widthOfBar bar =
    case bar of
        WideBar ->
            2

        NarrowBar ->
            1

        WideSpace ->
            0


{-| Barcode configuration.
-}
type alias Barcode msg =
    { svgAttributes : List (Attribute msg)
    , wrapperDivAttributes : List (Attribute msg)
    , barWidth : Float
    , barHeight : Float
    , barcode : String
    }


{-| Default barcode configuration.
-}
init : Barcode msg
init =
    { svgAttributes = []
    , wrapperDivAttributes = []
    , barWidth = 2
    , barHeight = 150
    , barcode = ""
    }


{-| Set the attributes for the <svg> element of the barcode.
-}
svgAttributes : List (Attribute msg) -> Barcode msg -> Barcode msg
svgAttributes a c =
    { c | svgAttributes = a }


{-| Set the attributes of the <div> wrapping the barcode and its label.
-}
wrapperDivAttributes : List (Attribute msg) -> Barcode msg -> Barcode msg
wrapperDivAttributes a c =
    { c | wrapperDivAttributes = a }


{-| Set the width of the barcode in px.
-}
barWidth : Float -> Barcode msg -> Barcode msg
barWidth w c =
    { c | barWidth = w }


{-| Set the height of the barcode in px.
-}
barHeight : Float -> Barcode msg -> Barcode msg
barHeight h c =
    { c | barHeight = h }


{-| Set the barcode text
-}
barcode : String -> Barcode msg -> Barcode msg
barcode b c =
    { c | barcode = b }


barWidthUnit : String
barWidthUnit =
    "px"


barHeightUnit : String
barHeightUnit =
    "px"


dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if predicate x then
                dropWhile predicate xs
            else
                list


{-| Render as a code 39 barcode
-}
code39BarcodeSvg : Barcode msg -> Maybe (Html msg)
code39BarcodeSvg ({ svgAttributes, wrapperDivAttributes, barWidth, barHeight, barcode } as c39b) =
    let
        fontSize =
            barHeight * 0.15

        textWidth =
            fontSize * toFloat (String.length barcode) * 1.1
    in
    code39BarcodeUnwrappedSvg c39b
        |> Maybe.map
            (\( svg, positions, width ) ->
                let
                    textBorder =
                        (width - textWidth) * 0.5

                    loffset =
                        case dropWhile (\pos -> pos < textBorder - barWidth) positions of
                            [] ->
                                0

                            x :: _ ->
                                x

                    roffset =
                        case dropWhile (\pos -> pos < width - textBorder) positions of
                            [] ->
                                0

                            x :: _ ->
                                width - x
                in
                div
                    (style
                        [ ( "position", "relative" )
                        , ( "width", toString width ++ "px" )
                        , ( "height", toString barHeight ++ barHeightUnit )
                        , ( "background", "white" )
                        ]
                        :: wrapperDivAttributes
                    )
                    [ svg
                    , div
                        [ style
                            [ ( "position", "absolute" )
                            , ( "left", "0" )
                            , ( "bottom", "0" )
                            , ( "width", toString width ++ "px" )
                            ]
                        ]
                        [ div
                            [ style
                                [ ( "width", toString (width - loffset - roffset) ++ barWidthUnit )
                                , ( "margin-left", toString loffset ++ barWidthUnit )
                                , ( "display", "table" )
                                , ( "transform", "translateY(50%)" )
                                , ( "text-align", "center" )
                                , ( "background", "white" )
                                , ( "color", "black" )
                                , ( "padding-bottom", "0" )
                                , ( "font-size", toString fontSize ++ barWidthUnit )
                                , ( "font-family", "monospace" )
                                , ( "letter-spacing", toString (fontSize * 0.4) ++ barWidthUnit )
                                , ( "user-select", "none" )
                                , ( "text-indent", "0.5ex" )
                                ]
                            ]
                            [ text barcode ]
                        ]
                    ]
            )


combine : List (Maybe a) -> Maybe (List a)
combine xs =
    let
        f e m =
            case ( e, m ) of
                ( Just x, Just xs ) ->
                    Just <| x :: xs

                _ ->
                    Nothing
    in
    List.foldr f (Just []) xs


code39BarcodeUnwrappedSvg : Barcode msg -> Maybe ( Html msg, List Float, Float )
code39BarcodeUnwrappedSvg { svgAttributes, barWidth, barHeight, barcode } =
    let
        mbars : Maybe (List Bar)
        mbars =
            Maybe.map List.concat <| combine <| List.map (\c -> Dict.get c code39) (String.toList barcode)
    in
    case mbars of
        Nothing ->
            Nothing

        Just bars ->
            let
                allBars =
                    List.concat [ code39StartStop, bars, code39StartStop ]

                fontSize =
                    round <| 0.1 * barHeight

                ( bs, totalWidth ) =
                    allBars
                        |> State.run 0
                        << State.traverse
                            (\bar ->
                                State.advance
                                    (\i ->
                                        case bar of
                                            WideSpace ->
                                                ( [], i + toFloat (widthOfBar NarrowBar) )

                                            _ ->
                                                ( [ ( i
                                                    , Svg.rect
                                                        [ Svg.Attributes.x (toString i ++ barWidthUnit)
                                                        , Svg.Attributes.y (toString 0)
                                                        , Svg.Attributes.width (toString (barWidth * toFloat (widthOfBar bar)) ++ barWidthUnit)
                                                        , Svg.Attributes.height (toString barHeight ++ barHeightUnit)
                                                        ]
                                                        []
                                                    )
                                                  ]
                                                , i + (barWidth * toFloat (widthOfBar bar)) + (barWidth * toFloat (widthOfBar NarrowBar))
                                                )
                                    )
                            )

                cbs =
                    List.concat bs

                svgBars =
                    cbs |> List.map (\( _, b ) -> b)

                svgBarPositions =
                    cbs |> List.map (\( p, _ ) -> p)
            in
            Just <|
                ( Svg.svg
                    (List.concat
                        [ [ Svg.Attributes.width (toString totalWidth ++ barWidthUnit)
                          , Svg.Attributes.height (toString barHeight ++ barHeightUnit)
                          , Svg.Attributes.version "1.1"
                          ]
                        , svgAttributes
                        ]
                    )
                    [ Svg.desc [] [ text ("*" ++ barcode ++ "*") ]
                    , Svg.g [ Svg.Attributes.fill "#000000" ] svgBars
                    ]
                , svgBarPositions
                , totalWidth
                )

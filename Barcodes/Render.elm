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
    = W -- Wide bar
    | N -- Narrow bar
    | S -- Wide space


code39 : Dict Char (List Bar)
code39 =
    Dict.fromList
        [ ( 'A', [ W, N, N, S, N, W ] )
        , ( 'B', [ N, W, N, S, N, W ] )
        , ( 'C', [ W, W, N, S, N, N ] )
        , ( 'D', [ N, N, W, S, N, W ] )
        , ( 'E', [ W, N, W, S, N, N ] )
        , ( 'F', [ N, W, W, S, N, N ] )
        , ( 'G', [ N, N, N, S, W, W ] )
        , ( 'H', [ W, N, N, S, W, N ] )
        , ( 'I', [ N, W, N, S, W, N ] )
        , ( 'J', [ N, N, W, S, W, N ] )
        , ( 'K', [ W, N, N, N, S, W ] )
        , ( 'L', [ N, W, N, N, S, W ] )
        , ( 'M', [ W, W, N, N, S, N ] )
        , ( 'N', [ N, N, W, N, S, W ] )
        , ( 'O', [ W, N, W, N, S, N ] )
        , ( 'P', [ N, W, W, N, S, N ] )
        , ( 'Q', [ N, N, N, W, S, W ] )
        , ( 'R', [ W, N, N, W, S, N ] )
        , ( 'S', [ N, W, N, W, S, N ] )
        , ( 'T', [ N, N, W, W, S, N ] )
        , ( 'U', [ W, S, N, N, N, W ] )
        , ( 'V', [ N, S, W, N, N, W ] )
        , ( 'W', [ W, S, W, N, N, N ] )
        , ( 'X', [ N, S, N, W, N, W ] )
        , ( 'Y', [ W, S, N, W, N, N ] )
        , ( 'Z', [ N, S, W, W, N, N ] )
        , ( '0', [ N, N, S, W, W, N ] )
        , ( '1', [ W, N, S, N, N, W ] )
        , ( '2', [ N, W, S, N, N, W ] )
        , ( '3', [ W, W, S, N, N, N ] )
        , ( '4', [ N, N, S, W, N, W ] )
        , ( '5', [ W, N, S, W, N, N ] )
        , ( '6', [ N, W, S, W, N, N ] )
        , ( '7', [ N, N, S, N, W, W ] )
        , ( '8', [ W, N, S, N, W, N ] )
        , ( '9', [ N, W, S, N, W, N ] )
        , ( ' ', [ N, S, W, N, W, N ] )
        , ( '-', [ N, S, N, N, W, W ] )
        , ( '$', [ N, S, N, S, N, S, N, N ] )
        , ( '%', [ N, N, S, N, S, N, S, N ] )
        , ( '.', [ W, S, N, N, W, N ] )
        , ( '/', [ N, S, N, S, N, N, S, N ] )
        , ( '+', [ N, S, N, N, S, N, S, N ] )
        ]


code39StartStop : List Bar
code39StartStop =
    [N,S,N,W,W,N]


widthOfBar : Bar -> Int
widthOfBar bar =
    case bar of
        W ->
            2

        N ->
            1

        S ->
            0


{-| Barcode configuration.
-}
type alias Barcode msg =
    { svgAttributes : List (Attribute msg)
    , wrapperDivAttributes : List (Attribute msg)
    , barWidth : Float
    , barHeight : Float
    , barcode : String
    , labelStyle : LabelStyle
    , includeDelimiters : Bool
    }


{-| Determines how the text below the barcode (if any) is displayed.
-}
type LabelStyle
    = NoLabel
    | LabelHalfBelow
    | LabelBelow


{-| Default barcode configuration.
-}
init : Barcode msg
init =
    { svgAttributes = []
    , wrapperDivAttributes = []
    , barWidth = 2
    , barHeight = 150
    , barcode = ""
    , labelStyle = LabelHalfBelow
    , includeDelimiters = True
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
code39BarcodeSvg ({ svgAttributes, wrapperDivAttributes, barWidth, barHeight, barcode, labelStyle, includeDelimiters } as c39b) =
    let
        fontSize =
            barHeight
                * (if includeDelimiters then
                    0.13
                   else
                    0.14
                  )

        withDelims =
            if includeDelimiters then
                "*" ++ barcode ++ "*"
            else
                barcode

        textWidth =
            fontSize * toFloat (String.length withDelims) * 1.1
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
                        , ( "display", "table" )
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
                                , ( "transform"
                                  , case labelStyle of
                                        LabelHalfBelow ->
                                            "translateY(50%)"

                                        _ ->
                                            "translateY(100%)"
                                  )
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
                            [ text withDelims ]
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
                                            S ->
                                                ( [], i + toFloat (widthOfBar N) )

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
                                                , i + (barWidth * toFloat (widthOfBar bar)) + (barWidth * toFloat (widthOfBar N))
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

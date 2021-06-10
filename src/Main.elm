module Main exposing (..)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (placeholder, style)
import Html.Events exposing (onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    String


init : Model
init =
    ""



-- UPDATE


type Msg
    = Input String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input text ->
            text



-- VIEW


view : Model -> Html Msg
view inputString =
    let
        maybeNumbers =
            inputString
                |> String.split ","
                |> List.map String.trim
                |> List.filter (\s -> s /= "" && s /= "-")

        numbers =
            List.filterMap String.toInt
                maybeNumbers

        correctSyntax =
            List.length maybeNumbers
                == List.length numbers
                && List.length numbers
                > 2

        product =
            if List.length maybeNumbers < 3 then
                ""

            else if correctSyntax then
                maxNumberProduct numbers |> String.fromInt

            else
                "incorrect syntax"
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-content" "center"
        , style "align-items" "center"
        , style "width" "100vw"
        , style "height" "100vh"
        ]
        [ input
            [ onInput Input
            , placeholder "1, 2, 3, 4"
            , style "width" "50%"
            , style "padding" "0rem"
            , style "border" "0.2rem solid rgba(0, 0, 0, 0.05)"
            , style "border-radius" "0.2rem"
            , style "outline" "none"
            , style "background" "rgba(255, 255, 255, 0.95)"
            , style "text-align" "center"
            , style "font-size" <|
                String.fromInt (100 // max (String.length inputString) 10)
                    ++ "vw"
            ]
            []
        , div
            [ style "position" "absolute"
            , style "z-index" "-1"
            , style "font-family" "monospace"
            , style "font-size" <|
                String.fromInt (100 // String.length product)
                    ++ "vw"
            ]
            [ text product ]
        ]


maxNumberProduct : List number -> number
maxNumberProduct input =
    let
        sorted =
            input
                |> List.sortWith
                    (\a ->
                        \b ->
                            case compare a b of
                                LT ->
                                    GT

                                EQ ->
                                    EQ

                                GT ->
                                    LT
                    )

        option1 =
            sorted
                |> List.take 3
                |> List.foldl (*) 1

        option2 =
            sorted
                |> List.drop (List.length sorted - 2)
                |> List.foldl (*) (Maybe.withDefault -1 <| List.head sorted)
    in
    max
        option1
        option2

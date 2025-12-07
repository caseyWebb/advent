module Day07 exposing (..)

import Browser
import Browser.Events
import Day07.Input
import Dict
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Lazy
import Maybe.Extra
import Parser exposing ((|.), (|=))
import Parser.Extra
import Set
import Zipper exposing (Zipper)


type alias Grid =
    Zipper (List Cell)


type Cell
    = Empty
    | Start
    | Beam Int
    | Splitter Bool


isSplitter : Cell -> Bool
isSplitter cell =
    case cell of
        Splitter _ ->
            True

        _ ->
            False


isBeam : Cell -> Bool
isBeam cell =
    case cell of
        Beam _ ->
            True

        _ ->
            False


parse : String -> Grid
parse =
    String.lines
        >> List.map
            (Parser.run
                (Parser.Extra.chars
                    (Parser.oneOf
                        [ Parser.Extra.const "." Empty
                        , Parser.Extra.const "S" Start
                        , Parser.Extra.const "|" (Beam 0)
                        , Parser.Extra.const "^" (Splitter False)
                        ]
                    )
                )
                >> Result.toMaybe
            )
        >> Maybe.Extra.values
        >> Zipper.fromList
        >> Maybe.Extra.require "Parse Error!"


step : Grid -> Grid
step =
    Zipper.stepWith
        (\current next ->
            let
                beams =
                    current
                        |> List.indexedMap Tuple.pair
                        |> List.filterMap
                            (\( i, cell ) ->
                                case cell of
                                    Start ->
                                        Just ( i, 1 )

                                    Beam intensity ->
                                        Just ( i, intensity )

                                    _ ->
                                        Nothing
                            )

                beamPositions =
                    beams
                        |> List.map Tuple.first
                        |> Set.fromList

                splitterPositions =
                    next
                        |> List.indexedMap Tuple.pair
                        |> List.filter (Tuple.second >> isSplitter)
                        |> List.map Tuple.first
                        |> Set.fromList

                newBeamPositions =
                    Set.foldl
                        (\splitterI acc ->
                            let
                                beamIntensity =
                                    Dict.get splitterI acc |> Maybe.withDefault 0

                                leftBeamIntensity =
                                    Dict.get (splitterI - 1) acc |> Maybe.withDefault 0

                                rightBeamIntensity =
                                    Dict.get (splitterI + 1) acc |> Maybe.withDefault 0
                            in
                            acc
                                |> Dict.remove splitterI
                                |> Dict.insert (splitterI - 1) (beamIntensity + leftBeamIntensity)
                                |> Dict.insert (splitterI + 1) (beamIntensity + rightBeamIntensity)
                        )
                        (Dict.fromList beams)
                        splitterPositions
            in
            List.indexedMap
                (\i cell ->
                    case Dict.get i newBeamPositions of
                        Just intensity ->
                            Beam intensity

                        Nothing ->
                            if isSplitter cell && Set.member i beamPositions then
                                Splitter True

                            else
                                cell
                )
                next
        )



--- Animate it! (Use elm reactor to view)


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { grid : Grid
    , playing : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { grid = initGrid
      , playing = False
      }
    , Cmd.none
    )


initGrid : Grid
initGrid =
    -- parse testInput
    parse Day07.Input.input


type Msg
    = Resume
    | Pause
    | Reset
    | Step


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resume ->
            ( { model | playing = True }, Cmd.none )

        Pause ->
            ( { model | playing = False }, Cmd.none )

        Reset ->
            ( { model | grid = initGrid }
            , Cmd.none
            )

        Step ->
            ( { model | grid = step model.grid }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        buttonStyles =
            [ Html.Attributes.style "background-color" "#0f0f23"
            , Html.Attributes.style "border" "none"
            , Html.Attributes.style "font-family" "monospace"
            , Html.Attributes.style "font-size" "18px"
            , Html.Attributes.style "color" "#090"
            , Html.Attributes.style "cursor" "pointer"
            ]
    in
    Html.div
        [ Html.Attributes.style "background-color" "#0f0f23"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "color" "#ccc"
        , Html.Attributes.style "font-family" "monospace"
        , Html.Attributes.style "min-height" "100vh"
        ]
        [ Html.div []
            [ Html.button (Html.Events.onClick Reset :: buttonStyles) [ Html.text "[Reset]" ]
            , Html.button (Html.Events.onClick Step :: buttonStyles) [ Html.text "[Step]" ]
            , if model.playing then
                Html.button (Html.Events.onClick Pause :: buttonStyles) [ Html.text "[Pause ||]" ]

              else
                Html.button (Html.Events.onClick Resume :: buttonStyles) [ Html.text "[Play |>]" ]
            ]
        , Html.div [] [ Html.strong [] [ Html.text "Beams: ", Html.text (String.fromInt (beamCount model.grid)) ] ]
        , Html.div [] [ Html.strong [] [ Html.text "Splits: ", Html.text (String.fromInt (splitCount model.grid)) ] ]
        , viewGrid model.grid
        ]


viewGrid : Grid -> Html Msg
viewGrid grid =
    Html.table [] (List.map viewRow (Zipper.toList grid |> List.reverse))


viewRow : List Cell -> Html Msg
viewRow =
    Html.Lazy.lazy
        (\row ->
            Html.tr [] (List.map viewCell row)
        )


viewCell : Cell -> Html Msg
viewCell =
    Html.Lazy.lazy
        (\cell ->
            Html.td
                (Html.Attributes.style "height" "17px"
                    :: (case cell of
                            Splitter True ->
                                [ Html.Attributes.style "text-shadow" "0 0 5px #f90, 0 0 10px #f90, 0 0 15px #f90"
                                ]

                            Splitter False ->
                                [ Html.Attributes.style "color" "#666"
                                ]

                            Beam intensity ->
                                [ Html.Attributes.style "color" "#f00"
                                , Html.Attributes.style "text-shadow" "0 0 5px #f00, 0 0 10px #f00, 0 0 15px #f00"
                                , Html.Attributes.title (String.fromInt intensity)
                                ]

                            _ ->
                                []
                       )
                )
                [ Html.text (cellToString cell) ]
        )


splitCount : Grid -> Int
splitCount =
    Zipper.toList >> List.concatMap identity >> List.filter ((==) (Splitter True)) >> List.length


beamCount : Grid -> Int
beamCount =
    Zipper.current
        >> List.filterMap
            (\cell ->
                case cell of
                    Beam intensity ->
                        Just intensity

                    _ ->
                        Nothing
            )
        >> List.sum


gridToString : Grid -> String
gridToString grid =
    String.join "\n" (List.map (List.map cellToString >> String.join "") (Zipper.toList grid |> List.reverse))


cellToString : Cell -> String
cellToString cell =
    case cell of
        Empty ->
            " "

        Start ->
            "S"

        Beam _ ->
            "|"

        Splitter used ->
            if used then
                "^"

            else
                "^"


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.playing then
        Browser.Events.onAnimationFrame (always Step)

    else
        Sub.none



---


testInput : String
testInput =
    String.trim """
.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
"""

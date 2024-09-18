module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (id)
import Html.Events exposing (onClick)
import Random exposing (generate)
import Random.Extra
import Random.List


main =
    Browser.element
        { init = \dimensions -> ( init ( 1, 1, 1 ), shuffleBombs dimensions )
        , update = \msg model -> update msg model |> (\m -> ( m, Cmd.none ))
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type Visibility
    = Visible
    | Hidden


type Field
    = Bomb
    | Empty


init ( numBombs, width, height ) =
    [ List.repeat numBombs ( Hidden, Bomb )
    , List.repeat ((width * height) - numBombs) ( Hidden, Empty )
    ]
        |> List.concat


shuffleBombs dimensions =
    generate GotShuffledlist (Random.List.shuffle <| init dimensions)


type Msg
    = ClickedOnField Int
    | GotShuffledlist (List (Visibility, Field))


update msg model =
    case msg of
        ClickedOnField target ->
            List.indexedMap (reveal target) model
        GotShuffledlist board ->
          board


reveal target i field =
    if target == i then
        case field of
            ( Hidden, content ) ->
                ( Visible, content )

            _ ->
                field

    else
        field


view fields =
    div [ id "board" ] <|
        List.indexedMap viewField fields


viewField i field =
    case field of
        ( Hidden, _ ) ->
            div [ onClick (ClickedOnField i) ] [ text "?" ]

        ( Visible, Empty ) ->
            div [] [ text "-" ]

        ( Visible, Bomb ) ->
            div [] [ text "x" ]

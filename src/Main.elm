module Main exposing (..)

import Html exposing (..)
import Browser
-- import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)


main =
  Browser.sandbox { init = init, update = update, view = view }


type alias Model =
  { tasks : List String,
    newTask: String
  }


init : Model
init =
  { tasks = [],
    newTask = ""
    }


type Msg
  = Add | Change String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Add ->
      { model | tasks = model.tasks ++ [ model.newTask ] , newTask = ""}
    Change newContent ->
      { model | newTask = newContent }


view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Task to Add", value model.newTask, onInput Change ] []
    , button [ onClick Add ] [ text "Add" ]
    , table [] 
        [ thead [] 
            [ th [] [text "Task"]
            , th [] [text "Done"]
            ]
          , tbody [] (List.map (\n -> tr [] [ td [] [ text n ], td [] [ text "X" ]]) model.tasks)
        ]
    ]


module Main exposing (..)

import Html exposing (..)
import Browser
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Set exposing (..)


main =
  Browser.sandbox { init = init, update = update, view = view }

-- Model

type alias Model =
  { tasks : List Task,
    newTask: String,
    uid: Int,
    taskDisplay: TaskDisplay,
    doneTasks: Set Int
  }

type TaskDisplay = TaskUpdate | DailyHabit | Report

type alias Task = 
  { name: String,
    active: Bool,
    id: Int
  }

init : Model
init =
  { tasks = [],
    newTask = "",
    uid = 1,
    taskDisplay = TaskUpdate,
    doneTasks = Set.empty 
  }


type Msg
  = Add 
  | Delete Int
  | Disable Int
  | Change String
  | DisplayUpdate TaskDisplay
  | CompleteTask Int


update : Msg -> Model -> Model
update msg model =
  case msg of
    DisplayUpdate taskUpdate ->
      { model | taskDisplay = taskUpdate}

    Add ->
      { model
        | tasks = model.tasks ++ [(Task model.newTask True model.uid)]
        , newTask = ""
        , uid = model.uid + 1
      }

    Delete id -> 
      { model | tasks = List.filter (\n -> n.id /= id) model.tasks}

    Disable id ->
      let
          disableTask t =
            if t.id == id then
              { t | active = not t.active }
            else
              t
      in
      { model | tasks = List.map disableTask model.tasks } 

    Change newContent ->
      { model | newTask = newContent }

    CompleteTask id ->
      { model | doneTasks = Set.insert id model.doneTasks }


view : Model -> Html Msg
view model =
  div []
  [ displayChangerView model
  , (
    case model.taskDisplay of
      TaskUpdate -> taskView model
      DailyHabit -> dailyHabit model
      Report -> dailyView model
    )
  ]


displayChangerView : Model -> Html Msg
displayChangerView model =
  div []
  [ button [ onClick (DisplayUpdate TaskUpdate) ] [ text "Edit Tasks" ]
  , button [ onClick (DisplayUpdate DailyHabit) ] [ text "Daily Habit" ]
  , button [ onClick (DisplayUpdate Report) ] [ text "Report" ]
  ]


taskView : Model -> Html Msg
taskView model = 
  div []
  [ input [ placeholder "Task to Add", value model.newTask, onInput Change ] []
  , button [ onClick Add ] [ text "Add" ]
  , viewTasks model.tasks
  ]


dailyHabit : Model -> Html Msg
dailyHabit model = 
  ul [] (List.map (\n -> dailyTaskShow n model.doneTasks) model.tasks)


dailyTaskShow : Task -> Set Int -> Html Msg
dailyTaskShow task doneTasks =
  li []
  [ text task.name
  , if Set.member task.id doneTasks then
      text ":)"
    else
      button [ onClick (CompleteTask task.id) ] [ text "Done" ]
  ]

dailyView : Model -> Html Msg
dailyView model =
  div []
  [ p [] [ text "Add daily view thing" ]
  ]

viewTable : Html Msg
viewTable = 
  div []
  [ table [] 
  [ thead [] 
  [ th [] [text "Task"]
  , th [] [text "Done"]
  ]
  , tbody [] 
  [ tr [] [ text "Meditate" ]
  , tr [] [ text "Another one" ]
  ]
  ]
  ]

viewTasks : List Task -> Html Msg
viewTasks tasks = 
  ul [] (List.map displayTask tasks)

displayTask : Task -> Html Msg
displayTask task =
  li []
    [ text task.name
    , button [ onClick (Disable task.id) ] [ text (if task.active then "Disable" else "Enable") ]
    , button [ onClick (Delete task.id) ] [ text "Delete" ]
    ]


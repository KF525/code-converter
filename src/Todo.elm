module Todo exposing (..)

import List exposing (map)
import Browser
import Html exposing (Html, button, div, input, li, text, ul)
import Html.Events exposing (onClick, onInput)


main = Browser.sandbox { init = init, update = update, view = view }

type alias TodoList = List String

init : TodoList
init = []

type Operation
  = AddTodo String
  | Finish

update : Operation -> TodoList -> TodoList

update op model =
  case op of
    AddTodo item ->
      item :: model
    Finish ->
      model

view : TodoList -> Html Operation
view todoList =
  div []
    [ button [ onClick Finish ] [ text "-" ]
    , ul [] (map (\item -> li [] [text item]) todoList)
    , input [ onInput (\inp -> AddTodo inp) ] []
    , button [ onClick (AddTodo "hi") ] [ text "+" ]
    ]

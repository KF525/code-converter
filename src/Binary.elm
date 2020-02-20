module Binary exposing (..)

import List exposing (map)
import Browser
import Html exposing (Html, div, text, input, ul, li)
import Browser
import String exposing (toList)
import Char exposing (toCode)
import Html.Events exposing (onInput)

main = Browser.sandbox { init = init, update = update, view = view }

type alias Text = String

init : Text
init = ""

type Msg = ToAscii String

update : Msg -> Text -> Text
update msg word =
    case msg of
        ToAscii w ->
         String.concat(List.map (\c -> String.fromInt(Char.toCode c)) (String.toList w))


view : Text -> Html Msg
view word =
    div []
       [ text word
       , input [ onInput (\inp -> ToAscii inp) ] [] ]
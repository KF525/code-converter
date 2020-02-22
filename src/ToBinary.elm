module ToBinary exposing (..)

import Html exposing (Html)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import List
import List.FlatMap exposing (flatMap)
import Browser
import Browser
import String
import Char
import Binary exposing (toIntegers, fromDecimal)

main = Browser.sandbox { init = init, update = update, view = view }

type alias Binary = (String, List Int)

init : Binary
init = ("", [])

type Msg = ToAscii | ToBinary | CaptureInput String

toAsciiHelper: String  -> List Int
toAsciiHelper w = List.map (\c -> Char.toCode c) (String.toList w)

toBinaryHelper: String -> List Int
toBinaryHelper w = flatMap (\i -> toIntegers(fromDecimal i)) (toAsciiHelper w)

update : Msg -> Binary -> Binary
update msg (inp, _)  =
    case msg of
        ToAscii -> (inp, toAsciiHelper inp)
        ToBinary -> (inp, toBinaryHelper inp)
        CaptureInput w -> (w, [])

view : Binary -> Html Msg
view (word, translation) =
    Html.div []
       [
       Html.input [ Html.Events.onInput (\inp -> CaptureInput inp), Html.Attributes.value word] []
       , Html.button [ Html.Events.onClick ToAscii ] [ Html.text "to ASCII"]
       , Html.button [ Html.Events.onClick ToBinary ] [ Html.text "to Binary"]
       , Html.ul [] (List.map (\item -> Html.li [] [Html.text (String.fromInt item)]) translation)
       ]
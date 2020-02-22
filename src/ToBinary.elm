module ToBinary exposing (..)

import List exposing (map)
import List.FlatMap exposing (flatMap)
import Browser
import Html exposing (Html, button, div, input, li, text, ul)
import Browser
import String exposing (toList, split)
import Char exposing (toCode)
import Html.Events exposing (onClick, onInput)
import Binary exposing (toIntegers, fromDecimal)

main = Browser.sandbox { init = init, update = update, view = view }

type alias Binary = (String, List Int)

init : Binary
init = ("", [])

type Msg = ToAscii String | ToBinary String | CaptureInput String

toAsciiHelper: String  -> List Int
toAsciiHelper w = map (\c -> Char.toCode c) (String.toList w)

toBinaryHelper: String -> List Int
toBinaryHelper w = flatMap (\i -> toIntegers(fromDecimal i)) (toAsciiHelper w)

update : Msg -> Binary -> Binary
update msg inp  =
    case msg of
        ToAscii w -> (w, toAsciiHelper w)
        ToBinary w -> (w, toBinaryHelper w)
        CaptureInput w -> (w, [])

view : Binary -> Html Msg
view word =
    div []
       [
       input [ onInput (\inp -> CaptureInput inp) ] []
       , button [ onClick (ToAscii (Tuple.first word))] [ text "to ASCII"]
       , button [ onClick (ToBinary (Tuple.first word))] [ text "to Binary"]
       , ul [] (map (\item -> li [] [text (String.fromInt item)]) (Tuple.second(word)))
       ]

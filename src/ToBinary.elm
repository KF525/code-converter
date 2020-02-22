module ToBinary exposing (..)

import Html exposing (Html)
import Html.Events
import Html.Attributes
import List
import List.FlatMap exposing (flatMap)
import Browser
import Browser
import String
import Char
import Binary

main = Browser.sandbox { init = init, update = update, view = view }

type alias Model = (String, List (List Int))

init : Model
init = ("", [])

type Msg = ToAscii | ToBinary | CaptureInput String

toWords: String -> List String
toWords = String.split " "

translator: List String -> (String -> List Int) -> List (List Int)
translator words codeConverter = List.map codeConverter words

concatList: List Int -> String
concatList list = String.concat (List.map (String.fromInt) list)

toAsciiHelper: String  -> List Int
toAsciiHelper word = List.map Char.toCode (String.toList word)

toBinaryHelper: String -> List Int
toBinaryHelper word = flatMap (\i -> Binary.toIntegers (Binary.fromDecimal i)) (toAsciiHelper word)

update : Msg -> Model -> Model
update msg (inp, _)  =
    case msg of
        ToAscii -> (inp, translator (toWords inp) toAsciiHelper)
        ToBinary -> (inp, translator (toWords inp) toBinaryHelper)
        CaptureInput w -> (w, [])

codeToHtmlMsg: List (List Int) -> List (Html Msg)
codeToHtmlMsg lists = List.map (toHtmlMsgHelper) (List.map (concatList) lists)

toHtmlMsgHelper: String -> Html Msg
toHtmlMsgHelper string = Html.div [] [Html.text string]

view : Model -> Html Msg
view (word, translation) =
    Html.div []
       [
       Html.input [ Html.Events.onInput (\inp -> CaptureInput inp), Html.Attributes.value word] []
       , Html.button [ Html.Events.onClick ToAscii ] [ Html.text "to ASCII"]
       , Html.button [ Html.Events.onClick ToBinary ] [ Html.text "to Binary"]
       , Html.div [] (codeToHtmlMsg translation)
       ]
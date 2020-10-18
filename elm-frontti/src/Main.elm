module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

import Article as A
import Creator as C
import DateTime exposing (DateTime)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { content : String
  }


init : Model
init =
  { content = "" }



-- UPDATE


type Msg
  = Change String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent }



-- VIEW


view : Model -> Html Msg
view model =
    let
        creator = C.Creator "feuer" "Feuer" "https://feuerx.net/etc/feuer.jpeg"
        article = A.Article creator [] "TESTI KONTENTTIA" [] 0 "TITLE" Nothing 0 [] 0 Nothing Nothing
    in 
        div []
            [ div [] [ text "Terveisiä tiedostosta Main.elm"]
            , div [] [ text "Tää on murjan mahdollisesti tuleva elm-frontti"]
            , div [] [ text "Testiartikkeli: "] 
            , A.view article
            ]

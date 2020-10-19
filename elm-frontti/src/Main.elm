module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http

import Article as A
import Creator as C
import DateTime exposing (DateTime)



-- MAIN

main : Program () Model Msg
main =
    Browser.element { init = init 
                    , update = update
                    , subscriptions = \_ -> Sub.none
                    , view = view }
    -- Browser.sandbox { init = init, update = update, view = view }



-- MODEL

-- Frontin tilat:

--   PageView [List Post]
-- | PostView Post



-- Tuetaan aluksi vain kaikkia lukuoperaatioita mitä nykyinen frontti (ts. postinäkymä ml. kommentit joita backend sattuu palauttamaan, sivunäkymä, otsikkopuu) tukee. Ei tehdä mitään editoria, pohditaan sitä sit joskus. 
type LoadableType
    = Post Int
    | Page Int 
      
type Model
    = PageView (List A.Article)
    | PostView A.Article
    | Loading LoadableType
    | ShowString String


init : () -> (Model, Cmd Msg)
init _ =
  ( Loading (Page 1)
  , getPage )



-- UPDATE


type Msg
  = SendHttpRequest
  | DataReceived (Result Http.Error String)

getPage : Cmd Msg
getPage =
    Http.get
        { url = "/api/posts/page/1/page-size/6"
        , expect = Http.expectString DataReceived}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SendHttpRequest ->
            (model, getPage)
        DataReceived result ->
            case result of
                Ok page_json ->
                    (ShowString page_json, Cmd.none)
                Err http_error ->
                    (ShowString "ERROR", Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
        case model of
            Loading type_ ->
                div [] [text "LOADING"]
            PostView articles ->
                div [] [text "ARTICLE"]
            PageView article ->
                div [] [text "PAGE"]
            ShowString str -> 
                div [] [text ("Showing a string of " ++ str)]

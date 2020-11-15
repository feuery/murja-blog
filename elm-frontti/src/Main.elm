module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div, input, text, pre, p, h2)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

import Html.Parser
import Html.Parser.Util

import Http

import Article as A
import Creator as C
import Page as P
import DateTime exposing (DateTime)

import Json.Decode as Decode



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
    = PageView P.Page
    | PostView A.Article
    | Loading LoadableType
    | ShowError String
      


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
                    case (Decode.decodeString P.pageDecoder page_json) of
                        Ok page -> 
                            (PageView page, Cmd.none)
                        Err error ->
                            (ShowError (Decode.errorToString error), Cmd.none)
                Err http_error ->
                    (ShowError "ERROR", Cmd.none)



-- VIEW

articleView : A.Article -> Html Msg
articleView article = div [] (List.concat [[ h2 [] [ text article.title ]],
                                          case (Html.Parser.run article.content) of 
                                              Ok content ->
                                                  Html.Parser.Util.toVirtualDom content
                                              Err error ->
                                                  [ p [] [text "VIRHE"]]])

view : Model -> Html Msg
view model =
        case model of
            Loading type_ ->
                div [] [text "LOADING"]
            PostView articles ->
                div [] [text "ARTICLE"]
            PageView page ->
                div [] (List.map articleView page.posts)
            ShowError err ->
                pre [] [text err]
            -- ShowString str -> 
            --     div [] [text ("Showing a string of " ++ str)]

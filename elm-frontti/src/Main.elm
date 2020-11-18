module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div, input, text, pre, p, h2, article, header, img, a)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

import Html.Parser
import Html.Parser.Util

import Http

import Article as A
import Creator as C
import Page as P
import Settings

import DateTime exposing (DateTime)
import Json.Decode as Decode
import DateFormat as Df
import Time
import Task



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
      
type ViewState
    = PageView P.Page
    | PostView A.Article
    | Loading LoadableType
    | ShowError String

type alias Model =
    { view : ViewState
    , settings : Maybe Settings.Settings}
    
type Msg
  = LoadPage
  -- | LoadSettings
  | PageReceived (Result Http.Error String)
  | SettingsReceived (Result Http.Error String)

-- init : () -> (Model, Cmd Msg)
init _ =
  ( Model (Loading (Page 1)) Nothing
  , getSettings
  )



-- UPDATE



getPage : Cmd Msg
getPage =
    Http.get
        { url = "/api/posts/page/1/page-size/6"
        , expect = Http.expectString PageReceived}

getSettings : Cmd Msg
getSettings =
    Http.get
        { url = "/api/settings/client-settings"
        , expect = Http.expectString SettingsReceived}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LoadPage ->
            (model, getPage)
        SettingsReceived result ->
            case result of
                Ok settings_json ->
                    case (Decode.decodeString Settings.settingsDecoder settings_json) of
                        Ok new_settings ->
                            ({model | settings = Just new_settings}, getPage)
                        Err error ->
                            ({model | view = ShowError (Decode.errorToString error)}, Cmd.none)
                Err http_error -> 
                    ({model | view = ShowError "Http error while loading settings"}, Cmd.none)
        PageReceived result ->
            case result of
                Ok page_json ->
                    case (Decode.decodeString P.pageDecoder page_json) of
                        Ok page -> 
                            ({model | view = PageView page}, Cmd.none)
                        Err error ->
                            ({model | view = ShowError (Decode.errorToString error)}, Cmd.none)
                Err http_error ->
                    ({model | view = ShowError "ERROR"}, Cmd.none)

-- Everything's now in utc
-- Getting user's local tz is fucking impossible due to static functional reasons
-- Let someone who cares fix this
formatDateTime formatString posixTime =
    Df.format formatString Time.utc posixTime
                        

-- VIEW

articleView : Settings.Settings -> A.Article -> Html Msg
articleView settings the_actual_post = div [class "post"] [ a [href ("/blog/post/" ++ String.fromInt the_actual_post.id)] [ text the_actual_post.title ],
                                                   div [class "meta"] [img [class "user_avatar", src the_actual_post.creator.img_location] [],
                                                                       p [] [text ("By " ++ the_actual_post.creator.nickname)],
                                                                           case the_actual_post.created_at of
                                                                               Just writing_time ->
                                                                                   p [] [text ("Written at " ++ (formatDateTime settings.time_format writing_time))]
                                                                               Nothing ->
                                                                                   p [] [text ("No idea when it's written")]],
                                                   article [class "content"] (case (Html.Parser.run the_actual_post.content) of 
                                                                                  Ok content ->
                                                                                      Html.Parser.Util.toVirtualDom content
                                                                                  Err error ->
                                                                                      [ p [] [text "VIRHE"]])]

view : Model -> Html Msg
view model =
    case model.settings of
        Just settings ->
            div [] [header [] [a [href "/"] [text settings.blog_title]],
                        div [id "container"] (List.concat ([ 
                                                   case model.view of
                                                       Loading type_ ->
                                                          [div [] [text "LOADING"]]
                                                       PostView articles ->
                                                          [div [] [text "ARTICLE"]]
                                                       PageView page ->
                                                          (List.map (articleView settings) page.posts)
                                                       ShowError err ->
                                                          [pre [] [text err]]
                                               , [div [id "sidebar"]
                                                   [ div [] [text "Meikä on sivupalkki :D"]]]
                                                ]))]
                                              
        Nothing ->
            div [] [text "Couldn't load settings"]

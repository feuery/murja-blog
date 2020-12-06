module Main exposing (..)

import Browser
import Html exposing (..)
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
import Dict.Extra exposing (groupBy)
import Dict exposing (toList, keys, get)
import String exposing (fromInt)
import String.Extra exposing (toSentenceCase)

import Browser.Navigation as Nav

import RouteParser
import Url


-- MAIN

main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


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
    , settings : Maybe Settings.Settings
    , key : Nav.Key
    , url : Url.Url}
    
type Msg
  = PageReceived (Result Http.Error String)
  -- | LoadPage 
  -- | LoadSettings
  | PostReceived (Result Http.Error String)
  | SettingsReceived (Result Http.Error String)
  | TitlesReceived (Result Http.Error String)
  | UrlChanged Url.Url
  | LinkClicked Browser.UrlRequest

-- init : () -> (Model, Cmd Msg)
init flags url key =
    let model = Model (case RouteParser.url_to_route url of
                           RouteParser.Page page_id -> (Loading (Page page_id))
                           RouteParser.Post post_id -> (Loading (Post post_id))
                           RouteParser.Home -> (Loading (Page 1))
                           RouteParser.NotFound -> (ShowError ("Couldn't parser url " ++ (Url.toString url)))) Nothing key url
    in
            ( model
            , getSettings)



-- UPDATE

getPage : Int -> Cmd Msg
getPage page_id =
    Http.get
        { url = "/api/posts/page/" ++ (fromInt page_id) ++ "/page-size/6"
        , expect = Http.expectString PageReceived}

getPost : Int -> Cmd Msg
getPost post_id =
    Http.get
        { url = "/api/posts/" ++ (fromInt post_id)
        , expect = Http.expectString PostReceived}

getSettings : Cmd Msg
getSettings =
    Http.get
        { url = "/api/settings/client-settings"
        , expect = Http.expectString SettingsReceived}

getTitles =
    Http.get
        { url = "/api/posts/titles"
        , expect = Http.expectString TitlesReceived}

loadPageOrPost : Model -> Cmd Msg
loadPageOrPost model =
    case model.view of
        Loading loadable_type ->
            case loadable_type of
                Post post_id ->
                    getPost post_id
                Page page_id ->
                    getPage page_id
        _ -> Cmd.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({settings} as model) =
    case msg of
        -- LoadPage ->
        --     (model, getPage)
        SettingsReceived result ->
            case result of
                Ok settings_json ->
                    case (Decode.decodeString Settings.settingsDecoder settings_json) of
                        Ok new_settings ->
                            ({model | settings = Just new_settings}, getTitles)
                        Err error ->
                            ({model | view = ShowError (Decode.errorToString error)}, Cmd.none)
                Err http_error -> 
                    ({model | view = ShowError "Http error while loading settings"}, Cmd.none)
        PostReceived result ->
            case result of
                Ok post_json ->
                    case (Decode.decodeString A.articleDecoder post_json) of
                        Ok post -> ({model | view = PostView post}, Cmd.none)
                        Err error -> ({model | view = ShowError (Decode.errorToString error)}, Cmd.none)
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
        TitlesReceived result ->
            case result of
                Ok json ->
                    case Decode.decodeString (Decode.list A.sidebarTitleDecoder) json of
                        Ok decoded_titles ->
                            case settings of
                                Just unwrapped_settings ->
                                    ({model | settings = Just {unwrapped_settings | titles = Just decoded_titles}}, loadPageOrPost model)
                                Nothing ->
                                    (model, Cmd.none)
                        Err error ->
                            ({model | view = ShowError (Decode.errorToString error)}, Cmd.none)
                Err error ->
                    ({model | view = ShowError "Coudln't load titles"}, Cmd.none)
        UrlChanged url ->
            ({model | url = url}, Cmd.none)
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    (model, Nav.pushUrl model.key (Url.toString url))
                Browser.External href ->
                    (model, Nav.load href)

-- Everything's now in utc
-- Getting user's local tz is fucking impossible due to static functional reasons
-- Let someone who cares fix this
formatDateTime formatString posixTime =
    Df.format formatString Time.utc posixTime
                        

-- VIEW



sidebarHistory : List A.Title -> Html Msg
sidebarHistory titles =
    let grouped_by_year = groupBy .year titles in
      div [id "grouper"]
          [ul []
               (List.concat (List.map (\year ->
                                           case get year grouped_by_year of
                                               Just per_year ->
                                                   [li [] [details [] [summary [] [text (fromInt year)],
                                                                        let grouped_by_month = groupBy .month per_year in
                                                                          ul [] (List.concat (List.map (\month ->
                                                                                                            [li [] [details [] [summary [] [text (toSentenceCase month)],
                                                                                                                                ul [class "title-list"] (titles
                                                                                                                                      |> List.filter (\title ->
                                                                                                                                                          title.year == year && title.month == month)
                                                                                                                                      |> List.map (\title ->
                                                                                                                                                       [li [class "title-list"]
                                                                                                                                                            [a [href ("/blog/post/" ++ (fromInt title.id))] [text title.title]]])
                                                                                                                                      |> List.concat)]]]) (keys grouped_by_month)))]]]

                                               Nothing ->
                                                        [li [] [text ("There's no year " ++ (fromInt year) ++ " in titles")]]) (keys grouped_by_year)))]
              
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

view : Model -> Browser.Document Msg
view model =
    { title = "Murja blog"      -- ¯\_(ツ)_/¯
    , body = 
        [case model.settings of
             Just settings ->
                 div [] [header [] [a [href "/"] [text settings.blog_title ]],
                         div [id "container"] (List.concat ([ 
                                                    case model.view of
                                                        Loading type_ ->
                                                          [div [] [text "LOADING"]]
                                                        PostView article ->
                                                          [articleView settings article]
                                                        PageView page ->
                                                          (List.map (articleView settings) page.posts)
                                                        ShowError err ->
                                                          [pre [] [text err]]
                                                   , [div [id "sidebar"] (
                                                                          case settings.titles of
                                                                              Just titles ->
                                                                                [ sidebarHistory titles ]
                                                                              Nothing ->
                                                                                [div [] [text "Loading history failed"]])]]))]
             Nothing ->
                 div [] [text "Couldn't load settings"]]}

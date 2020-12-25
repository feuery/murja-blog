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
import Message exposing (..)
import User

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




viewStatePerUrl url =
    case RouteParser.url_to_route url of
                           RouteParser.Page page_id -> (Loading (Page page_id))
                           RouteParser.Post post_id -> (Loading (Post post_id))
                           RouteParser.Home -> (Loading (Page 1))
                           RouteParser.NotFound -> (ShowError ("Couldn't parser url " ++ (Url.toString url)))
    
-- init : () -> (Model, Cmd Msg)
init flags url key =
    let model = Model (viewStatePerUrl url) Nothing LoggedOut key url
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
        { url = "/api/posts/post/" ++ (fromInt post_id)
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

postLogin username password =
    Http.post
       { url = "/api/login/login"
       , expect = Http.expectString LoginSuccess
       , body = Http.stringBody "application/json" ("{\"username\": \""++username++"\", \"password\": \""++password++"\"})")}
                
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
            ({model | url = url, view = viewStatePerUrl url}, getSettings)
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    (model, Nav.pushUrl model.key (Url.toString url))
                Browser.External href ->
                    (model, Nav.load href)
        LoginFocus ->
            ({model | loginState = LoggingIn "" ""}, Cmd.none)
        ChangeUsername username ->
            case model.loginState of
                LoggingIn old_username password ->
                    ({model | loginState = LoggingIn username password}, Cmd.none)
                _ -> (model, Cmd.none)
        ChangePassword password ->
            case model.loginState of
                LoggingIn username old_password ->
                    ({model | loginState = LoggingIn username password}, Cmd.none)
                _ -> (model, Cmd.none)
        DoLogIn -> case model.loginState of
                       LoggingIn username password ->
                           (model, postLogin username password)
                       _ -> (model, Cmd.none)
        LoginSuccess result ->
            case result of
                Ok login_result ->
                    case Decode.decodeString User.userDecoder login_result of
                        Ok user ->
                            ({model | loginState = LoggedIn user}, Cmd.none)
                        Err error ->
                             ({model | loginState = LoginFailed}, Cmd.none)
                Err error ->
                    ({model | loginState = LoginFailed}, Cmd.none)
                           

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
                                                        [li [] [text ("There's no year " ++ (fromInt year) ++ " in titles")]]) (keys grouped_by_year |> List.reverse)))]
              
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
    case model.settings of
        Nothing ->
            { title = "Error loading murja"
            , body = 
                  [div [] [text "Couldn't load settings"]]}
        Just settings ->
            { title = settings.blog_title
            , body = 
                  [div [] [header [] [a [href "/"] [text settings.blog_title ]],
                               div [id "container"] (List.concat ([ 
                                                          case model.view of
                                                              Loading type_ ->
                                                                [div [] [text "LOADING"]]
                                                              PostView article ->
                                                                [articleView settings article]
                                                              PageView page ->
                                                                [div [] (List.concat [(List.map (articleView settings) page.posts),
                                                                                          [footer [] (if page.id > 1 then [a [href ("/blog/page/" ++ fromInt (page.id + 1))] [text "Older posts"],
                                                                                                                               a [href ("/blog/page/" ++ fromInt (page.id - 1)), class "newer-post"] [text "Newer posts"]]
                                                                                                      else [a [href ("/blog/page/" ++ fromInt (page.id + 1))] [text "Next page"]])]])]
                                                              
                                                              ShowError err ->
                                                                [pre [] [text err]]
                                                         , [div [id "sidebar"] [User.loginView model.loginState,
                                                                                 (case settings.titles of
                                                                                    Just titles ->
                                                                                        sidebarHistory titles 
                                                                                    Nothing ->
                                                                                        div [] [text "Loading history failed"])]
                                                                ]]))]]}

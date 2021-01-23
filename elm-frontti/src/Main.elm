port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

import Html.Parser
import Html.Parser.Util

import Http

import Article
import Ajax_cmds exposing (..)
import Creator as C
import Page as P
import Settings
import Message exposing (..)
import User
import Topbar
import PostsAdmin
import PostEditor

import DateTime exposing (DateTime)
import Json.Decode as Decode
import DateFormat as Df
import Time
import Task
import Dict.Extra exposing (groupBy)
import Dict exposing (toList, keys, get)
import String exposing (fromInt)
import String.Extra exposing (toSentenceCase)
import Stack exposing (push, top, pop)

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
  tags ReceivedTag




viewStatePerUrl url =
    case RouteParser.url_to_route url of
                           RouteParser.Page page_id -> (Loading (Page page_id))
                           RouteParser.Post post_id -> (Loading (Post post_id))
                           RouteParser.Home -> (Loading (Page 1))
                           RouteParser.NotFound -> (ShowError ("Couldn't parse url " ++ (Url.toString url)))
                           --TODO fix 
                           RouteParser.PostEditor post_id -> Loading (Post post_id)
    
init flags url key =
    let model = Model (push (viewStatePerUrl url) Stack.initialise) Nothing LoggedOut key url
    in
        ( model
        , Cmd.batch [ getSettings
                    , getTitles
                    , getSession
                    ])



-- UPDATE


-- PORTS --
port prompt : String -> Cmd msg
port alert : String -> Cmd msg
port tags : (String -> msg) -> Sub msg
                
loadPageOrPost : Model -> Cmd Msg
loadPageOrPost model =
    case top model.view_stack of
        Nothing -> Cmd.none
        Just current_view -> case current_view of
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
                            ({model | settings = Just new_settings}, Cmd.none)
                        Err error ->
                            ({model | view_stack = push (ShowError (Decode.errorToString error)) model.view_stack}, Cmd.none)
                Err http_error -> 
                    ({model | view_stack = push (ShowError "Http error while loading settings") model.view_stack}, Cmd.none)
        PostReceived result ->
            case result of
                Ok post_json ->
                    case (Decode.decodeString Article.articleDecoder post_json) of
                        Ok post -> ({model | view_stack = push (PostView post) model.view_stack}, Cmd.none)
                        Err error -> ({model | view_stack = push (ShowError (Decode.errorToString error)) model.view_stack}, Cmd.none)
                Err http_error ->
                    ({model | view_stack = push (ShowError "Http error while loading settings") model.view_stack}, Cmd.none)
        PageReceived result ->
            case result of
                Ok page_json ->
                    case (Decode.decodeString P.pageDecoder page_json) of
                        Ok page -> 
                            ({model | view_stack = push (PageView page) model.view_stack}, Cmd.none)
                        Err error ->
                            ({model | view_stack = push (ShowError (Decode.errorToString error)) model.view_stack}, Cmd.none)
                Err http_error ->
                    ({model | view_stack = push (ShowError "ERROR") model.view_stack}, Cmd.none)
        TitlesReceived result ->
            case result of
                Ok json ->
                    case Decode.decodeString (Decode.list Article.sidebarTitleDecoder) json of
                        Ok decoded_titles ->
                            case settings of
                                Just unwrapped_settings ->
                                    ({model | settings = Just {unwrapped_settings | titles = Just decoded_titles}}, loadPageOrPost model)
                                Nothing ->
                                    (model, Cmd.none)
                        Err error ->
                            ({model | view_stack = push (ShowError (Decode.errorToString error)) model.view_stack}, Cmd.none)
                Err error ->
                    ({model | view_stack = push (ShowError "Coudln't load titles") model.view_stack}, Cmd.none)
        UrlChanged url ->
            ({model | url = url, view_stack = push (viewStatePerUrl url) model.view_stack}, getSettings)
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
        GotSession result ->
            case result of
                Ok user ->
                    ({model | loginState = LoggedIn user}, Cmd.none)
                Err error ->
                    case error of
                        Http.BadStatus status ->
                            if status == 401 then
                                -- no valid session
                                (model, Cmd.none)
                            else
                                ({model | view_stack = push (ShowError ("Error (" ++ String.fromInt status ++ ") when loading session")) model.view_stack}, Cmd.none)
                        Http.BadBody err ->
                            ({model | view_stack = push (ShowError ("Error when loading session: " ++ err)) model.view_stack}, Cmd.none)
                        _ -> ({model | view_stack = push (ShowError "Error when loading session") model.view_stack}, Cmd.none)
        EditableTitlesReceived result ->
            case result of
                Ok titles_json ->
                    case Decode.decodeString (Decode.list Article.sidebarTitleDecoder) titles_json of
                        Ok titles ->
                            ({model | view_stack = push (PostEditorList titles) model.view_stack}, Cmd.none)
                        Err error ->
                            ({model | view_stack = push (ShowError "Coudln't load titles") model.view_stack}, Cmd.none)
                Err error ->
                    ({model | view_stack = push (ShowError "http error while loading titles") model.view_stack}, Cmd.none)
        OpenPostEditor post_id ->
            (model, getPostEditorData post_id)
        EditorPostReceived result ->
            case result of
                Ok post ->
                    ({model | view_stack = push (PostEditor post "") model.view_stack}, Cmd.none)
                Err _ ->
                    ({model | view_stack = push (ShowError "Error while loading editor") model.view_stack}, Cmd.none)
        ChangeViewState viewstate cmd ->
            let command = 
                    case cmd of
                        Just cmd_ -> cmd_
                        Nothing -> Cmd.none
            in
                ({model | view_stack = push viewstate model.view_stack}, command)
        PopViewstate ->
            let (_, new_stack) = pop model.view_stack in
            ({model | view_stack = new_stack}, Cmd.none)
        PromptTag prompt_message ->
            (model, prompt prompt_message)
        Alert alert_msg ->
            (model, alert alert_msg)
        SelectTag tag ->
            let (top_viewstate, stack) = pop model.view_stack in
            case top_viewstate of
                Just (PostEditor article selected_tag) -> 
                    ({model | view_stack = push (PostEditor article tag) stack}, Cmd.none)
                _ -> (model, Cmd.none)
        ReceivedTag tag ->
            case top model.view_stack of
                Just (PostEditor article selected_tag) ->
                    let (_, new_stack) = pop model.view_stack in
                    ({model | view_stack = push (PostEditor {article | tags = tag :: article.tags } selected_tag) new_stack}, Cmd.none)
                _ -> ({model | view_stack = push (ShowError "Error while loading editor") model.view_stack}, Cmd.none)
        DropTag tag ->
            case top model.view_stack of
                Just (PostEditor article selected_tag) ->
                    let (_, new_stack) = pop model.view_stack in
                    ({model | view_stack = push (PostEditor {article | tags = List.filter ((/=) tag) article.tags } selected_tag) new_stack}, Cmd.none)
                _ -> ({model | view_stack = push (ShowError "Error dropping tag") model.view_stack}, Cmd.none)
                    
                           
getContentCmd viewState =
    case viewState of
        PostEditorList _ -> getEditablePosts
        _ -> Cmd.none
                     
-- Everything's now in utc
-- Getting user's local tz is fucking impossible due to static functional reasons
-- Let someone who cares fix this
formatDateTime formatString posixTime =
    Df.format formatString Time.utc posixTime
                        

-- VIEW



sidebarHistory : List Article.Title -> Html Msg
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
              
articleView : Settings.Settings -> Article.Article -> Html Msg
articleView settings the_actual_post = div [class "post"] [ a [href ("/blog/post/" ++ String.fromInt the_actual_post.id)] [ text the_actual_post.title ],
                                                   div [class "meta"] [User.user_avatar the_actual_post.creator,
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
                  [ header [] [a [href "/"] [text settings.blog_title ]]
                  , Topbar.topbar model.loginState
                  , div [class "flex-container"] 
                        [ div [class "page"]
                              (let maybe_view  = top model.view_stack in
                              case maybe_view of
                                   Nothing -> [div [] [text "Couldn't load view status"]]
                                   Just viewstate ->
                                      case viewstate of
                                          Loading type_ ->
                                              [div [] [text "LOADING"]]
                                          PostView article ->
                                              [articleView settings article]
                                          PageView page ->
                                              (List.concat [(List.map (articleView settings) page.posts),
                                                                [footer [] (if page.id > 1 then [ a [href ("/blog/page/" ++ fromInt (page.id + 1))] [text "Older posts"]
                                                                                                , a [href ("/blog/page/" ++ fromInt (page.id - 1)), class "newer-post"] [text "Newer posts"]]
                                                                            else [a [href ("/blog/page/" ++ fromInt (page.id + 1))] [text "Next page"]])]])
                                          ShowError err ->
                                              [pre [] [text err]]
                                          PostEditorList titles -> [ PostsAdmin.view titles ]
                                          PostEditor post tag_index -> PostEditor.postEditor post tag_index
                                          CommentsList -> [ div [] [text "CommentsList"] ]
                                          MediaList -> [div [] [text "Medialist!"]])
                        , div [id "sidebar"] [ User.loginView model.loginState
                                            , (case settings.titles of
                                                   Just titles ->
                                                       sidebarHistory titles 
                                                   Nothing ->
                                                       div [] [text "Loading history failed"])]]]}

port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

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
import Medialist exposing (medialist)
import Image
import ImageSelector exposing (imageSelector)

import DateTime exposing (DateTime)
import Json.Decode as Decode
import Json.Encode
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
import Date_utils exposing (int_to_month_string)

import UUID
import File exposing (mime)


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
subscriptions _ = Sub.batch 
                  [ tags ReceivedTag
                  , aceStateUpdate AceStateUpdate]

initialModel url key viewstate = Model viewstate Nothing False False [] Nothing LoggedOut key url Nothing Time.utc
    
viewStatePerUrl : Url.Url -> (ViewState, List (Cmd Msg))
viewStatePerUrl url =
    case RouteParser.url_to_route url of
        RouteParser.Page page_id -> (Loading, [ getSettings
                                              , getTitles
                                              , getSession
                                              , getPage page_id
                                              ])
        RouteParser.Post post_id -> (Loading, [ getSettings
                                              , getTitles
                                              , getSession
                                              , getPost post_id])
        RouteParser.Home -> (Loading, [ getSettings
                                      , getTitles
                                      , getSession
                                      , getPage 1
                                      ])
        RouteParser.PostEditor post_id -> (Loading, [ getSettings
                                                    , getTitles
                                                    , getSession
                                                    , getPostEditorData post_id])
        RouteParser.PostAdmin -> (Loading, [ getSettings
                                           , getSession
                                           , getTitles
                                           , getEditablePosts ])
        RouteParser.MediaManager -> (Loading, [ getSettings
                                              , getSession
                                              , getTitles
                                              , getListOfImages True] )
        RouteParser.TaggedPosts tags_ -> (Loading, [ getSession
                                                   , getSettings
                                                   , getTitles
                                                   , loadTaggedPosts tags_])
        RouteParser.NewPost ->
            (PostEditor, [ getSettings
                         , getTitles
                         , getSession])
                               
        RouteParser.NotFound -> (ShowError ("Couldn't parse url " ++ (Url.toString url)), [Cmd.none])
    
init _ url key =
    let (viewstate, cmds) = (viewStatePerUrl url)
        model = initialModel url key viewstate
    in
        ( model
        , Cmd.batch (List.append cmds [ Task.perform AdjustTimeZone Time.here]))


-- UPDATE


-- PORTS --
port prompt : String -> Cmd msg
port alert : String -> Cmd msg
port tags : (String -> msg) -> Sub msg
port aceStateUpdate : (String -> msg) -> Sub msg               
                
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SettingsReceived result ->
            case result of
                Ok settings_json ->
                    case (Decode.decodeString Settings.settingsDecoder settings_json) of
                        Ok new_settings ->
                            ({model | settings = Just new_settings}, Cmd.none)
                        Err error ->
                            ( model
                            , alert ("Error loading settings " ++ Debug.toString error))
                Err http_error ->
                    ( model
                    , alert ("Error loading settings " ++ Debug.toString http_error))
        PostReceived result ->
            case result of
                Ok post_json ->
                    case (Decode.decodeString Article.articleDecoder post_json) of
                        Ok post -> ( {model | view_state = PostView post}
                                   , Cmd.none)
                        Err error -> ( model
                                     , alert ("Error loading post " ++ Debug.toString error))
                Err http_error ->
                    ( model
                    , alert ("HttpError loading post " ++ Debug.toString http_error))
        PageReceived result ->
            case result of
                Ok page_json ->
                    case (Decode.decodeString P.pageDecoder page_json) of
                        Ok page -> 
                            ( {model | view_state = PageView page}
                            , Cmd.none)
                        Err error ->
                            ( model
                            , alert ("Error loading page " ++ Debug.toString error))
                Err http_error ->
                    ( model
                    , alert ("HttpError loading page " ++ Debug.toString http_error))
        TitlesReceived result ->
            case result of
                Ok json ->
                    case Decode.decodeString (Decode.list Article.sidebarTitleDecoder) json of
                        Ok decoded_titles ->
                            case model.settings of
                                Just unwrapped_settings ->
                                    ({model | settings = Just {unwrapped_settings | titles = Just decoded_titles}}, Cmd.none)
                                Nothing ->
                                    (model, Cmd.none)
                        Err error ->
                            ( model
                            , alert ("Error loading titles " ++ Debug.toString error))
                Err error ->
                    ( model
                    , alert ("Error loading titles " ++ Debug.toString error))
        UrlChanged url ->
            let (view_state, cmds) = viewStatePerUrl url in 
            ({model | url = url, view_state = view_state}, Cmd.batch cmds)
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
                    if model.view_state == PostEditor then
                        ({ model | loginState = LoggedIn user 
                         , postEditorSettings = Just (PostEditorSettings (Article.Article (C.Creator user.username user.nickname user.img_location) [""] "" Nothing "New post" Nothing Nothing [] Nothing Nothing) "")}
                        , Cmd.none)
                    else 
                        ({model | loginState = LoggedIn user}, Cmd.none)
                Err error ->
                    case error of
                        Http.BadStatus status ->
                            if status == 401 then
                                -- no valid session
                                (model, Cmd.none)
                            else
                                ( model
                                , alert ("Error (" ++ String.fromInt status ++ ") when loading session"))
                        Http.BadBody err ->
                            ( model
                            , alert ("Error when loading session: " ++ err))
                        _ -> ( model
                             , alert ("Error when loading session"))
        EditableTitlesReceived result ->
            case result of
                Ok titles_json ->
                    case Decode.decodeString (Decode.list Article.sidebarTitleDecoder) titles_json of
                        Ok titles ->
                            ({model | view_state = PostEditorList titles}
                            , Cmd.none)
                        Err error ->
                            ( model
                            , alert ("Coudln't load titles " ++ Debug.toString error))
                Err error ->
                    ( model
                    , alert ("EditableTitlesReceived error " ++ Debug.toString error))
        OpenPostEditor post_id ->
            (model, getPostEditorData post_id)
        EditorPostReceived result ->
            case result of
                Ok post ->
                    ({ model | view_state = PostEditor
                     , postEditorSettings = Just (PostEditorSettings post "")}
                    , Cmd.none)
                Err error ->
                    ( model
                    , alert ("Error loading post editor " ++ Debug.toString error))
        PromptTag prompt_message ->
            (model, prompt prompt_message)
        Alert alert_msg ->
            (model, alert alert_msg)
        RunAce content ->
            (model, reallySetupAce content)
        SelectTag tag ->
            case model.postEditorSettings of
                Just settings ->
                    ({ model | postEditorSettings = Just
                           { settings | selected_tag = tag}}
                    , Cmd.none)
                _ -> (model, Cmd.none)
        ReceivedTag tag ->
            case model.postEditorSettings of
                Just settings ->
                    let article = settings.article in
                    ({ model | postEditorSettings = Just
                           { settings | article = { article | tags = tag :: settings.article.tags}
                           }}
                    , Cmd.none)
                Nothing -> (model, alert "ReceivedTag called even though postEditorSettings is nil")
        DropTag tag ->
            case model.postEditorSettings of
                Just settings ->
                    let article = settings.article in
                    ({ model | postEditorSettings = Just
                           { settings | article =
                                 { article | tags = List.filter ((/=) settings.selected_tag) article.tags}}}
                    , Cmd.none)
                Nothing -> (model, alert "DropTag called even though postEditorSettings is nil")
        HttpIgnoreResponse result ->
            (model, Cmd.none)
        ChangePost new_content ->
            case model.postEditorSettings of
                Just settings ->
                    let article = settings.article in
                    ({ model | postEditorSettings = Just
                           { settings | article =
                                 { article | content = new_content}}}
                    , Cmd.none)
                Nothing -> (model, alert "ChangePost called even though postEditorSettings is nil")
        SavePost article ->
            let new_post_p = article.id == Nothing in
            doGoHome_
              { model | postEditorSettings = Nothing}
              [ if new_post_p then postArticle article else putArticle article ]
                    

        GoHome -> doGoHome model
        HttpGoHome _ -> doGoHome model

        AceStateUpdate content ->
            case model.postEditorSettings of
                Just settings ->
                    let article = settings.article in
                    ({ model | postEditorSettings = Just
                           { settings | article =
                                 { article | content = content}}}
                    , Cmd.none)
                Nothing -> (model, alert "AceStateUpdate called even though postEditorSettings is nil")
                    
        ChangeTitle new_title ->
            case model.postEditorSettings of
                Just settings ->
                    let article = settings.article in
                    ({ model | postEditorSettings = Just
                           { settings | article =
                                 { article | title = new_title}}}
                    , Cmd.none)
                Nothing -> (model, alert "ChangeTitle called even though postEditorSettings is nil")            
        HttpManagerGetListOfImages _ -> (model, getListOfImages True)                                  
        GetListOfImages -> ( { model | showImageModal = True }
                           , getListOfImages False)
        GotListOfImages managerCalled result ->
            case result of
                Ok json ->
                    case Decode.decodeString (Decode.list Image.imageDecoder) json of
                        Ok images ->
                            case managerCalled of
                                True ->
                                    ({ model
                                         | loadedImages = images
                                         , view_state = MediaList
                                         , medialist_state = Just (MediaListState [] Dict.empty)}
                                    , Cmd.batch (List.map (\image -> getReferencingPosts (UUID.toString image.id)) images))
                                False -> 
                                    ({model | showImageModal = True, loadedImages = images}, Cmd.none)
                        Err error ->
                            ( model
                            , alert (Debug.toString error))
                Err error ->
                    ( model
                    , alert (Debug.toString error))
        SelectedImage img_id ->
            ( {model | showImageModal = False, loadedImages = [] }
            , addImgToAce (UUID.toString img_id))
        EditorDragEnter ->
            ( {model | draggingImages = True}
            , Cmd.none)
        EditorDragLeave ->
            ( {model | draggingImages = False}
            , Cmd.none)
        GotFiles file files ->
            if String.startsWith "image" (mime file) then
                ( { model | draggingImages = False }
                , postPicture file)
            else
                ( { model | draggingImages = False }
                , alert ("Got " ++ (mime file) ++ ", expected an image"))
        GotInputFiles files ->
            if List.all (\file -> String.startsWith "image" (mime file)) files then
                ( model
                , Cmd.batch (List.map (\file -> postPicture file) files))
            else
                ( model
                , alert ("Expected images, got " ++ (String.join ", " (List.map mime files))))
        UploadedImage imgResponse ->
            case imgResponse of
                Ok actualResponse ->
                    ( model
                    , addImgToAce (UUID.toString actualResponse.id ))
                Err err ->
                    (model, alert ("Error uploading image " ++ Debug.toString err))
        MarkImageForRemoval img_id ->
            case model.medialist_state of
                Just state ->
                    if List.member img_id state.selected_ids_for_removal then
                        ({ model | medialist_state = Just {state | selected_ids_for_removal =
                                                               List.filter ((/=) img_id) state.selected_ids_for_removal}}
                        , Cmd.none)
                    else 
                        ({ model | medialist_state = Just {state | selected_ids_for_removal = img_id :: state.selected_ids_for_removal}}
                        , Cmd.none)
                        
                Nothing ->
                    ( model
                    , alert "Medialist state is uninitialized")
        MarkAllImages ids ->
            case model.medialist_state of
                Just state ->
                    ({ model | medialist_state = Just {state | selected_ids_for_removal = ids}}
                    , Cmd.none)
                Nothing -> ( model
                           , alert "Medialist state is uninitialized")
        RemoveSelectedImages ->
            case model.medialist_state of
                Just state -> 
                    (model, deletePictures state.selected_ids_for_removal)
                Nothing -> (model, Cmd.none)
        GotReferencingPosts response ->
            case response of
                Ok posts ->
                    case model.medialist_state of
                        Just state -> ({ model | medialist_state = Just {state | referencing_posts =
                                                                             Dict.union state.referencing_posts (groupBy .media_id posts)}}
                                      , Cmd.none)
                        Nothing -> ( model
                                   , Cmd.none)
                Err err ->
                    ( model
                    , alert "Error while downloading info about referencing posts, check your devtools' network log")
        PushUrl url ->
            ( model, Nav.pushUrl model.key url )
        AdjustTimeZone zone ->
            ( {model | zone = zone}
            , Cmd.none)
        GotTaggedPosts result ->
            case result of
                Ok posts ->
                    ({ model | view_state = TaggedPostsView posts}
                    , Cmd.none)
                Err err ->
                    ( model , alert ( "Error loading tagged posts " ++ (Debug.toString err)))
            
                  
            
doGoHome_ model other_cmds =
    (model, Cmd.batch (List.append [ getSettings
                                   , getTitles
                                   , getSession
                                   , getPage 1
                                   , Nav.pushUrl model.key "/blog/"]
                           other_cmds))

doGoHome model = doGoHome_ model []        
                           
                
getContentCmd viewState =
    case viewState of
        PostEditorList _ -> getEditablePosts
        _ -> Cmd.none
                     
formatDateTime formatString zone posixTime =
    Df.format formatString zone posixTime
                        

-- VIEW



sidebarHistory : List Article.Title -> Html Msg
sidebarHistory titles =
    let grouped_by_year = groupBy .year titles in
      div [id "grouper"]
          [ul []
               (List.concat (List.map (\year ->
                                           case get year grouped_by_year of
                                               Just per_year ->
                                                   [li [] [details [] [summary [] [text ((fromInt year) ++ " (" ++ (fromInt (List.length per_year)) ++ ")")],
                                                                        let grouped_by_month = groupBy .month per_year in
                                                                          ul [] (List.concat (List.map (\month ->
                                                                                                            case (int_to_month_string month) of
                                                                                                                Just month_str ->
                                                                                                                    let month_titles = titles |> List.filter (\title ->
                                                                                                                                                                  title.year == year && title.month == month)
                                                                                                                    in
                                                                                                                        [li [] [details [] [summary [] [text ((toSentenceCase month_str) ++ " (" ++ (fromInt (List.length month_titles)) ++ ")")]
                                                                                                                               , ul [class "title-list"] (month_titles
                                                                                                                                                         |> List.map (\title ->
                                                                                                                                                                          [li [class "title-list"]
                                                                                                                                                                               [a [href ("/blog/post/" ++ (fromInt title.id))] [text title.title]]])
                                                                                                                                                         |> List.concat)]]]
                                                                                                                Nothing -> [li [] [details [] [summary [] [text ("Couldn't decode month " ++ (String.fromInt month))]]]]
                                                                                                       ) (keys grouped_by_month) |> List.reverse))]]]

                                               Nothing ->
                                                        [li [] [text ("There's no year " ++ (fromInt year) ++ " in titles")]]) (keys grouped_by_year |> List.reverse)))]

dangerouslySetInnerHTML: String -> Attribute msg
dangerouslySetInnerHTML = Json.Encode.string >> Html.Attributes.property "dangerouslySetInnerHTML"

articleView settings loginstate zone the_actual_post =
    case the_actual_post.id of
        Nothing -> div [class "post"] [text "Post id is nil :/"]
        Just post_id ->
            div [class "post"] [ a [href ("/blog/post/" ++ String.fromInt post_id)] [ text the_actual_post.title ]
                               , div [class "meta"] [ User.user_avatar the_actual_post.creator
                                                    , p [] [text ("By " ++ the_actual_post.creator.nickname)]
                                                    , case the_actual_post.created_at of
                                                          Just writing_time ->
                                                              p [] [text ("Written at " ++ (formatDateTime settings.time_format zone writing_time))]
                                                          Nothing ->
                                                              p [] [text ("No idea when it's written")]]
                               , (case loginstate of
                                      LoggedIn _ -> a [ href ("/blog/post/edit/" ++ String.fromInt post_id)
                                                      , onClick (OpenPostEditor post_id)] [text "Edit this post"]
                                      _ -> div [] [])
                                                    
                               , article [ class "content"
                                         , dangerouslySetInnerHTML the_actual_post.content ] []]

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
                              (case model.view_state of
                                           Loading ->
                                               [div [] [text "LOADING"]]
                                           PostView article ->
                                               [ articleView settings model.loginState model.zone article ]
                                           PageView page ->
                                               (List.concat [(List.map (articleView settings model.loginState model.zone) page.posts),
                                                                 [footer [] (if page.id > 1 then [ a [href ("/blog/page/" ++ fromInt (page.id + 1))] [text "Older posts"]
                                                                                                 , a [href ("/blog/page/" ++ fromInt (page.id - 1)), class "newer-post"] [text "Newer posts"]]
                                                                             else [a [href ("/blog/page/" ++ fromInt (page.id + 1))] [text "Next page"]])]])
                                           ShowError err ->
                                               [pre [] [text err]]
                                           PostEditorList titles -> [ PostsAdmin.view titles ]
                                           TaggedPostsView articles ->
                                               (List.map (articleView settings model.loginState model.zone) articles)
                                           PostEditor ->
                                               case model.postEditorSettings of
                                                   Just editorSettings ->
                                                       let post = editorSettings.article
                                                           tag_index = editorSettings.selected_tag in
                                                       PostEditor.postEditor post tag_index model.showImageModal model.loadedImages model.draggingImages
                                                   Nothing -> [ div [] [ text "No post loaded" ]]
                                           MediaList -> [ medialist model.loadedImages model.medialist_state ])
                        , div [id "sidebar"] [ User.loginView model.loginState
                                             , (case settings.titles of
                                                    Just titles ->
                                                        sidebarHistory titles 
                                                    Nothing ->
                                                        div [] [text "Loading history failed"])
                                             , (case model.view_state of
                                                    PostEditorList titles -> PostsAdmin.tagList titles
                                                    
                                                    _ -> div [] [])]]]}

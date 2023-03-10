module Ajax_cmds exposing (..)

import Article
import User
import Page
import Message exposing (..)
import Http exposing (..)
import Image as Image
import Settings
import Json.Decode as Json

getSession =
    Http.get
        { url = "/api/login/session"
        , expect = Http.expectJson GotSession User.userDecoder}

getEditablePosts : Cmd Msg
getEditablePosts =
    Http.get
        { url = "/api/posts/all-titles"
        , expect = Http.expectJson EditableTitlesReceived (Json.list Article.sidebarTitleDecoder) }

getPage : Int -> Cmd Msg
getPage page_id =
    Http.get
        { url = "/api/posts/page/" ++ (String.fromInt page_id) ++ "/page-size/6"
        , expect = Http.expectJson PageReceived Page.pageDecoder}

getPost : Int -> Cmd Msg
getPost post_id =
    Http.get
        { url = "/api/posts/post/" ++ (String.fromInt post_id)
        , expect = Http.expectJson PostReceived Article.articleDecoder}

getSettings : Cmd Msg
getSettings =
    Http.get
        { url = "/api/settings/client-settings"
        , expect = Http.expectJson SettingsReceived Settings.settingsDecoder}

getTitles =
    Http.get
        { url = "/api/posts/titles"
        , expect = Http.expectJson TitlesReceived (Json.list Article.sidebarTitleDecoder)}

postLogin username password =
    Http.post
       { url = "/api/login/login"
       , expect = Http.expectJson LoginSuccess User.userDecoder
       , body = Http.jsonBody <| User.encodeLoggingIn <| User.UserLoggingIn username password}

getPostEditorData post_id =
    Http.get
        { url = "/api/posts/post/" ++ (String.fromInt post_id) ++ "/allow-hidden/true"
        , expect = Http.expectJson EditorPostReceived Article.articleDecoder}

postArticle : Article.Article -> Cmd Msg        
postArticle article =
    Http.post
        { url = "/api/posts/post"
        , body = Http.jsonBody <| Article.encode article
        , expect = Http.expectString HttpIgnoreResponse }
        
putArticle : Article.Article -> Cmd Msg        
putArticle article =
    case article.id of
        Just id ->
            Http.request
                { method = "PUT"
                , headers = []
                , url = "/api/posts/post"
                , body = Http.jsonBody <| Article.encode article
                , expect = Http.expectString HttpGoHome
                , timeout = Nothing
                , tracker = Nothing
                }
        Nothing -> Cmd.none

-- returns { :id :name }
getListOfImages : Bool -> Cmd Msg
getListOfImages managerCalled = Http.get
                  { url = "/api/pictures/list/all"
                  , expect = Http.expectJson (GotListOfImages managerCalled) (Json.list Image.imageDecoder)}


postPicture pictureFile = Http.post 
                          { url = "/api/pictures"
                          , body = Http.multipartBody [ Http.filePart "file" pictureFile ]
                          , expect = Http.expectJson UploadedImage Image.imageResponseDecoder }


deletePictures ids = Http.request
                     { url = "/api/pictures"
                     , method = "DELETE"
                     , headers = []
                     , expect = Http.expectString HttpManagerGetListOfImages
                     , body = Http.jsonBody <| (Image.list_of_uuids_encode ids)
                     , timeout = Nothing
                     , tracker = Nothing}

getReferencingPosts id = Http.get
                         { url = "/api/pictures/referencing/" ++ id
                         , expect = Http.expectJson GotReferencingPosts (Json.list Image.referencingPostDecoder)}

loadTaggedPosts tags = Http.get
                       { url = "/api/posts/tagged/" ++ tags
                       , expect = Http.expectJson GotTaggedPosts (Json.list Article.articleDecoder)}

loadPostVersion post_id_int version_id_int =
    let post_id = String.fromInt post_id_int
        version_id = String.fromInt version_id_int in
    Http.get
        { url = "/api/posts/post/" ++ post_id ++ "/version/" ++ version_id
        , expect = Http.expectJson GotOldPost Article.articleDecoder}

module Ajax_cmds exposing (..)

import Article
import User
import Message exposing (..)
import Http exposing (..)
import Image as Image
import Json.Decode as Json

getSession =
    Http.get
        { url = "/api/login/session"
        , expect = Http.expectJson GotSession User.userDecoder}

getEditablePosts : Cmd Msg
getEditablePosts =
    Http.get
        { url = "/api/posts/all-titles"
        , expect = Http.expectString EditableTitlesReceived }

getPage : Int -> Cmd Msg
getPage page_id =
    Http.get
        { url = "/api/posts/page/" ++ (String.fromInt page_id) ++ "/page-size/6"
        , expect = Http.expectString PageReceived}

getPost : Int -> Cmd Msg
getPost post_id =
    Http.get
        { url = "/api/posts/post/" ++ (String.fromInt post_id)
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
                  , expect = Http.expectString (GotListOfImages managerCalled)}


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

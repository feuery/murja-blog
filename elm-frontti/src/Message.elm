port module Message exposing (..)

import Http
import Browser
import Time
import Page as P
import Article
import Browser.Navigation as Nav
import Settings
import Url
import Title
import Image exposing (Image, ReferencingPost)

import File exposing (File)
import UUID exposing (UUID)
import Stack exposing (..)
import Dict exposing (Dict)
    
type ViewState
    = PageView P.Page
    | PostView Article.Article
    | Loading 
    | ShowError String
    | PostEditorList (List Title.Title)                     -- list all the posts in db
    | PostEditor
    | MediaList                     -- list all the image blobs in db
      
type alias User =
    { username : String
    , nickname : String
    , img_location : String
    }

type LoginState
    = LoggedIn LoginUser
    | LoggingIn String String
    | LoginFailed
    | LoggedOut      


type alias LoginUser =
    { nickname : String
    , username : String
    , img_location : String
    , primary_group_name : String
    , permissions : List String
    }

type alias MediaListState =
    { selected_ids_for_removal : List UUID
    , referencing_posts : Dict String (List ReferencingPost)}

type alias PostEditorSettings =
    { article : Article.Article
    , selected_tag : String }
    
type alias Model =
    { view_state : ViewState
    , settings : Maybe Settings.Settings
    , showImageModal : Bool
    , draggingImages : Bool
    , loadedImages : List Image
    , medialist_state : Maybe MediaListState
    , loginState : LoginState
    , key : Nav.Key
    , url : Url.Url
    , postEditorSettings: Maybe PostEditorSettings
    , zone : Time.Zone}
    
type Msg
  = PageReceived (Result Http.Error String)
  | PostReceived (Result Http.Error String)
  | SettingsReceived (Result Http.Error String)
  | TitlesReceived (Result Http.Error String)
  | EditableTitlesReceived (Result Http.Error String)
  | UrlChanged Url.Url
  | LinkClicked Browser.UrlRequest
  | LoginFocus
  | ChangeUsername String
  | ChangePassword String
  | DoLogIn
  | LoginSuccess (Result Http.Error String)
  | GotSession (Result Http.Error LoginUser)
  | OpenPostEditor Int
  | EditorPostReceived (Result Http.Error Article.Article)
  | PromptTag String
  | ReceivedTag String
  | AceStateUpdate String
  | SelectTag String
  | Alert String
  | DropTag String
  | SavePost Article.Article
  | HttpIgnoreResponse (Result Http.Error String)
  | ChangePost String
  | HttpGoHome (Result Http.Error String)
  | GoHome
  | ChangeTitle String
  | RunAce String
  | GetListOfImages
  | GotListOfImages Bool (Result Http.Error String)
  | SelectedImage UUID
  | EditorDragEnter
  | EditorDragLeave
  | GotFiles File (List File)
  | GotInputFiles (List File)
  | UploadedImage (Result Http.Error Image.PostImageResponse)
  | MarkImageForRemoval UUID
  | MarkAllImages (List UUID)
  | RemoveSelectedImages
  | HttpManagerGetListOfImages (Result Http.Error String)
  | GotReferencingPosts (Result Http.Error (List Image.ReferencingPost))
  | PushUrl String
  | AdjustTimeZone Time.Zone
  


-- ports
port reallySetupAce : String -> Cmd msg
port addImgToAce : String -> Cmd msg

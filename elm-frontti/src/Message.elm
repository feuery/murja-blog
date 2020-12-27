module Message exposing (..)

import Http
import Browser
import Page as P
import Article as A
import Browser.Navigation as Nav
import Settings
import Url
import Title 

type LoadableType
    = Post Int
    | Page Int      
    
type ViewState
    = PageView P.Page
    | PostView A.Article
    | Loading LoadableType
    | ShowError String

type AdminViewState
    = Regular                   -- delegate to whatever model.view says
    | Posts (List Title.Title)                     -- list all the posts in db
    | Comments                  -- list all the comments in db
    | Media                     -- list all the image blobs in db
      
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
    , img_location : String
    , primary_group_name : String
    , permissions : List String
    }      
      
type alias Model =
    { view : ViewState
    , adminView : AdminViewState
    , settings : Maybe Settings.Settings
    , loginState : LoginState
    , key : Nav.Key
    , url : Url.Url}
    
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
  | ChangeAdminViewState AdminViewState
  | GotSession (Result Http.Error LoginUser)

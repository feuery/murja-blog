module Message exposing (..)

import Http
import Browser
import Page as P
import Article as A
import Browser.Navigation as Nav
import Settings
import Url

type LoadableType
    = Post Int
    | Page Int      
      
type ViewState
    = PageView P.Page
    | PostView A.Article
    | Loading LoadableType
    | ShowError String

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
    , userid : Int
    , primary_group_name : String
    , permissions : List String
    }      
      
type alias Model =
    { view : ViewState
    , settings : Maybe Settings.Settings
    -- , loggedInUser : Maybe LoginUser
    , loginState : LoginState
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
  | LoginFocus
  | ChangeUsername String
  | ChangePassword String
  | DoLogIn
  | LoginSuccess (Result Http.Error String)

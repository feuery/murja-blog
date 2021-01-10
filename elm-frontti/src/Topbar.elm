module Topbar exposing (..)

import Message exposing (..)
import User
import Ajax_cmds exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

topbar state =
    case state of
        LoggedIn user ->
            div [class "left-sidebar"] [ span [] [text ("Welcome, " ++ user.nickname)]
                                       , User.user_avatar user
                                       , ul [] [ li [onClick PopViewstate] [text "Home"]
                                               , li [onClick (ChangeViewState (PostEditorList []) (Just getEditablePosts))] [text "Manage posts"]
                                               , li [onClick (ChangeViewState CommentsList Nothing)] [text "Manage comments"]
                                               , li [onClick (ChangeViewState MediaList Nothing)] [text "Manage media"]]
                                       ]
        _ -> div [] []

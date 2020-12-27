module Topbar exposing (..)

import Message exposing (..)
import User

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


topbar state =
    case state of
        LoggedIn user ->
            div [class "left-sidebar"] [ span [] [text ("Welcome, " ++ user.nickname)]
                                       , User.user_avatar user
                                       , ul [] [ li [onClick (ChangeAdminViewState Regular)] [text "Home"]
                                               , li [onClick (ChangeAdminViewState (Posts []))] [text "Manage posts"]
                                               , li [onClick (ChangeAdminViewState Comments)] [text "Manage comments"]
                                               , li [onClick (ChangeAdminViewState Media)] [text "Manage media"]]
                                       ]
        _ -> div [] []

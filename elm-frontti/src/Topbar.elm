module Topbar exposing (..)

import Message exposing (..)
import User
import Article exposing (..)
import Creator exposing (..)
import Ajax_cmds exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Browser.Navigation as Nav

import Button exposing (murja_button)

topbar state =
    case state of
        LoggedIn user ->                               
            div [class "left-sidebar"] [ span [] [text ("Welcome, " ++ user.nickname)]
                                       , User.user_avatar user
                                       , ul [] [ li [] [ murja_button [ onClick GoHome] [text "Home"]]
                                               , li [] [ murja_button [ onClick (PushUrl "/blog/postadmin") ] [text "Manage posts"]]
                                               , li [] [ murja_button [ onClick (PushUrl "/blog/mediamanager")] [text "Manage media"]]
                                               , li [] [ murja_button [ onClick (PushUrl "/blog/new_post") ] [text "New post!"]]]]
        _ -> div [] []

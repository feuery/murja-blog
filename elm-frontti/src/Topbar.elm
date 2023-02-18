module Topbar exposing (..)

import Message exposing (..)
import User
import Article exposing (..)
import Creator exposing (..)
import Ajax_cmds exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

topbar state =
    case state of
        LoggedIn user ->
            let empty_article = Article (Creator user.username user.nickname user.img_location) [""] "" Nothing "New post" Nothing Nothing [] Nothing Nothing
                new_post_cmd = (ChangeViewState (PostEditor empty_article "") Nothing) in
            div [class "left-sidebar"] [ span [] [text ("Welcome, " ++ user.nickname)]
                                       , User.user_avatar user
                                       , ul [] [ li [ onClick GoHome] [text "Home"]
                                               , li [ onClick (ChangeViewState (PostEditorList []) (Just getEditablePosts))] [text "Manage posts"]
                                               , li [ onClick (ChangeViewState CommentsList Nothing)] [text "Manage comments"]
                                               , li [ onClick (ChangeViewState MediaList Nothing)] [text "Manage media"]
                                               , li [ onClick new_post_cmd ] [text "New post!"]]]
        _ -> div [] []

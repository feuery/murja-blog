module Topbar exposing (..)

import Message exposing (..)
import User
import Article exposing (..)
import Creator exposing (..)
import Ajax_cmds exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Button exposing (murja_button)

topbar state =
    case state of
        LoggedIn user ->
            let empty_article = Article (Creator user.username user.nickname user.img_location) [""] "" Nothing "New post" Nothing Nothing [] Nothing Nothing
                new_post_cmd = (ChangeViewState (PostEditor empty_article "") Nothing) in
            div [class "left-sidebar"] [ span [] [text ("Welcome, " ++ user.nickname)]
                                       , User.user_avatar user
                                       , ul [] [ li [] [ murja_button [ onClick GoHome] [text "Home"]]
                                               , li [] [ murja_button [ onClick (ChangeViewState (PostEditorList []) (Just getEditablePosts))] [text "Manage posts"]]
                                               , li [] [ murja_button [ onClick (ManagerGetListOfImages)] [text "Manage media"]]
                                               , li [] [ murja_button [ onClick new_post_cmd ] [text "New post!"]]]]
        _ -> div [] []

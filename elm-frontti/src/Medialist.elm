module Medialist exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Dict
    

import Http

import Article
import Ajax_cmds exposing (..)
import Creator as C
import Page as P
import Message exposing (..)
import ImageSelector exposing (imageSelector)
import Image

import UUID

referencing_post_view post = li []
                             [ a [ href ("/blog/post/" ++ (String.fromInt post.post_id)) ]
                                 [ text post.post_title ]]

medialist images medialist_state =
    case medialist_state of
        Just state ->
            div [ class "vertical-flex-container" ]
                (List.append
                     [div [class "title-flex-container"]
                          [ button [ class "post-admin-title"
                                   , onClick RemoveSelectedImages ] [ text "Remove selected" ]
                          , div [ class "post-admin-title" ] []
                          , button [ class "post-admin-title"
                                   , onClick (MarkAllImages (List.map .id images))] [ text "Select all" ]]]
                     (List.map (\image ->
                                    let checkbox_id = "delete" ++ (UUID.toString image.id)
                                    in 
                                        div [ class "title-flex-container" ]
                                    [ details [ ]
                                          [ summary [ class "post-admin-title" ] [h2 [] [ text image.name ]
                                                                                 , div [] [ text (UUID.toString image.id)]]
                                          , ImageSelector.image image]
                                    , details [ class "post-admin-title" ]
                                        [ summary [] [ text "Referencing posts" ]
                                        , case (Dict.get (UUID.toString image.id) state.referencing_posts) of
                                              Just referencing_posts ->
                                                  if referencing_posts == [] then
                                                      div [] [ text "No referencing posts" ]
                                                  else
                                                       ul []
                                                           (List.map referencing_post_view referencing_posts)
                                              Nothing -> div [] [ text "No referencing posts" ]]
                                    , div [ class "post-admin-title" ]
                                        [ label [for checkbox_id] [text "Choose for deletion"]
                                        , input [ type_ "checkbox"
                                                , id checkbox_id
                                                , checked (List.member image.id state.selected_ids_for_removal)
                                                , onClick (MarkImageForRemoval image.id)] []]])
                          images))
        Nothing ->
            div [] [ text "lol et sit oo initialisoinu medialist_statea" ]
                       
                       

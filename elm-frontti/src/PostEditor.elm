module PostEditor exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
    

import Http

import Article
import Ajax_cmds exposing (..)
import Creator as C
import Page as P
import Message exposing (..)
import ImageSelector exposing (imageSelector)

optionize tag = option [value tag] [text tag]

tagView post selectedTag = div [class "tagview"]
              [ select [ multiple True
                        , class "tag-select"
                        , onInput SelectTag] (List.map optionize post.tags)
               , button [ onClick (PromptTag "New tag? ") ]
                   [ text "Add tag"]
               , button [ onClick (DropTag selectedTag) ]
                   [text "Remove selected tag"]
               ]

postEditor post tag showImageModal loadedImages = [ div [ id "editor-buttons"]
                                                        [ input [ name "title"
                                                                , id "editor-post-title"
                                                                , value post.title
                                                                , onInput ChangeTitle] []
                                                        , button [ id "editor-post-save"
                                                                 , onClick SavePost ] [text "Save version"]
                                                        , button [ id "image-insert-btn"
                                                                 , onClick GetListOfImages]
                                                              [text "Insert image"]]
                                                        
                                                  , tagView post tag
                                                  , if showImageModal then imageSelector loadedImages else div [] []
                                                  , div [ id "editor-post-content"] []]
                  

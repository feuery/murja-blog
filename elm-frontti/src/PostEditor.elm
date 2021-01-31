module PostEditor exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Http

import Article
import Ajax_cmds exposing (..)
import Creator as C
import Page as P
import Message exposing (..)

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

postEditor post tag = [ div [] [ input [ name "title"
                                       , id "editor-post-title"
                                       , value post.title
                                       , onInput ChangeTitle] []]
                  , div [] [ button [ id "editor-post-save"
                                    , onClick SavePost ] [text "Save version"]]
                  , tagView post tag
                  , textarea [ id "editor-post-content"
                             , onInput ChangePost] [text post.content]
                  ]
                  

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
                      -- I'm not exactly happy with this hack
                      -- but if I return a cmd that runs the ace initialisation code from the state-change
                      -- that also initiates this view, ace code is run before this #editor-post-content div
                      -- exists in DOM
                      --
                      -- I was also unable to find a on-rendered event I could've hooked the RunAce command
                      -- so for now ace is initiated by on-click event on an element whose existence is certain
                  , div [ onClick (RunAce post.content)] [text "Click here to load editor"]
                  , div [ id "editor-post-content"
                        ] [] -- [text post.content]
                  ]
                  

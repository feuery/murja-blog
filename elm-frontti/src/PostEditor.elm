module PostEditor exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

import Http

import Article
import Ajax_cmds exposing (..)
import Creator as C
import Page as P

optionize tag = option [value tag] [text tag]

tagView post = div [class "tagview"]
               [ select [ multiple True
                        , class "tag-select"] (List.map optionize post.tags)
               , button [] [text "Add tag"]
               , button [] [text "Remove selected tag"]
               ]

postEditor post = [ div [] [ input [name "title", id "editor-post-title", value post.title] []]
                  , div [] [ button [id "editor-post-save"] [text "Save version"]]
                  , tagView post
                  , textarea [id "editor-post-content"] [text post.content]
                  ]
                  

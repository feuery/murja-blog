module PostEditor exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

import Http

import Article
import Ajax_cmds exposing (..)
import Creator as C
import Page as P

postEditor post = [ input [name "title", id "editor-post-title", value post.title] []
                  , button [id "editor-post-save"] [text "Save version"]
                  , textarea [id "editor-post-content"] [text post.content]]
                  

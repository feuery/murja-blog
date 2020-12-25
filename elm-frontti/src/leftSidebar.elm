module LeftSidebar exposing (..)

import Message exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


leftSidebar state =
    case state of
        LoggedIn user ->
            div [class "left-sidebar"] [text ("Welcome, " ++ user.nickname)]
        _ -> div [] []

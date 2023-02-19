module ImageSelector exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import UUID

import Image exposing (Image)
import Message exposing (..)

image the_actual_img = img [ src ("/api/pictures/" ++ (UUID.toString the_actual_img.id))
                           , onClick (SelectedImage the_actual_img.id)] []

imageSelector : List Image -> Html Msg
imageSelector img_list =
    div [ id "selector-div" ]
        (List.map image img_list)

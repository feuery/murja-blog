module ImageSelector exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import UUID

import Image exposing (Image)
import Message exposing (..)

imageSelector : List Image -> Html Msg
imageSelector img_list =
    div [ id "selector-div" ]
        (List.map (\image -> img [ src ("/api/pictures/" ++ (UUID.toString image.id))
                                 , onClick (SelectedImage image.id)
                                 ]
                       []) img_list)

module PostsAdmin exposing (..)

import Message exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

titleView title = div [class "title-flex-container" ]
                  [ span [class "post-admin-title" ] [text ( title.title ++ " - " ++ title.month ++ ", " ++ (String.fromInt title.year))]
                  , a [ href ("/blog/post/edit/" ++ String.fromInt title.id)
                      , onClick (OpenPostEditor title.id)] [text "Edit"]
                  , a [href ("/blog/post/remove/" ++ String.fromInt title.id)] [text "Remove"]]

view titles = (div [class "vertical-flex-container"]
               (titles |>
                List.map titleView))
                              

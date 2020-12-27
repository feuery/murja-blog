module PostsAdmin exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

titleView title = div [class "title-flex-container" ]
                  [ span [class "post-admin-title" ] [text ( title.title ++ " - " ++ title.month ++ ", " ++ (String.fromInt title.year))]
                  , a [] [text "Edit"]
                  , a [] [text "Remove"]]

view titles = (div [class "vertical-flex-container"]
               (titles |>
                List.map titleView))
                              

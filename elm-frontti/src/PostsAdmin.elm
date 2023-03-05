module PostsAdmin exposing (..)

import Message exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Date_utils exposing (int_to_month_string)

titleView title =  case (int_to_month_string title.month) of
                       Just month -> 
                           div [ class "title-flex-container" ] 
                               [ span [class "post-admin-title" ] [text ( title.title ++ " - " ++ month  ++ ", " ++ (String.fromInt title.year))]
                               , a [ href ("/blog/post/edit/" ++ String.fromInt title.id)
                                   , onClick (OpenPostEditor title.id)] [text "Edit"]
                               , a [href ("/blog/post/remove/" ++ String.fromInt title.id)] [text "Remove"]
                               , div [class "post-admin-title" ]
                                   (List.append  [ h3 [] [text "Tags: "]]
                                        (List.map (\tag -> a [ href ("/blog/tags/" ++ tag)
                                                             , style "display" "block" ]
                                                       [ text tag ]) title.tags))]
                       Nothing -> div [] [text ("Parsing month " ++ (String.fromInt title.month) ++ " failed")]

view titles = (div [class "vertical-flex-container"]
               (titles |>
                List.map titleView))
                              

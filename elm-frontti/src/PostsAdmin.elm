module PostsAdmin exposing (..)

import Message exposing (..)
import Set

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Date_utils exposing (int_to_month_string)

tagListElement allTags tag =
    let count = (  allTags
                |> List.filter ((==) tag)
                |> List.length) in
    a [ href ("/blog/tags/" ++ tag)
      , style "display" "block" ]
    [ text (tag ++ " (" ++ (String.fromInt count) ++ ")")]

tagList titles =
    let allTags = (  titles
                  |> List.concatMap (\title -> title.tags))
        tags = (  allTags
               |> Set.fromList
               |> Set.toList
               |> List.filter ((/=) "")) in
    div [] (List.append [h3 [] [text ("Tags in the system (" ++ String.fromInt (List.length tags) ++ "): ")]]
                (  tags 
                |> List.map (tagListElement allTags)))

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
                              

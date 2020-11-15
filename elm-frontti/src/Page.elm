module Page exposing (..)


import Http
import Html exposing (Html, text, pre)
import Article as A

import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Decode.Extra as Extra

type alias Page =
    { last_page: Bool
    , posts: List A.Article}

pageDecoder : Decoder Page
pageDecoder =
    Decode.map2 Page
        (Decode.field "last-page?" Decode.bool)
        (Decode.field "posts" (Decode.list A.articleDecoder))

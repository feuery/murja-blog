module Article exposing (..)

import DateTime exposing (DateTime)

import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Decode.Extra as Extra
import Time

-- {
--   "tags": [],
--   "creator": {
--     "username": "feuer",
--     "nickname": "Feuer",
--     "img_location": "https://feuerx.net/etc/feuer.jpeg"
--   },
--   "content": "<p>Tämä on testi posti :D</p>\n\n<p>Uusi paragraaaaaaaafffi</p>",
--   "comments": [],
--   "amount-of-comments": 0,
--   "title": "Testi Posti",
--   "prev-post-id": null,
--   "id": 1,
--   "versions": [],
--   "version": null,
--   "next-post-id": null,
--   "created_at": "2020-10-16T07:52:59Z"
-- }

import Creator exposing (Creator, creatorDecoder)

decodeApply : Decode.Decoder a -> Decode.Decoder (a -> b) -> Decode.Decoder b
decodeApply value partial =
    Decode.andThen (\p -> Decode.map p value) partial


type alias Article =
    { creator : Creator
    , tags : List String
    , content : String
    -- TODO make a comment type
    , comments : Maybe (List String)
    -- , amount_of_comments : Int
    , title : String
    , pre_post_id : Maybe Int
    , id : Int
    , versions: List Int
    , version : Maybe Int
    -- , next_post_id: Maybe Int
    , created_at: Maybe Time.Posix
    }

    
tagsDecoder = Decode.field "tags" (Decode.list Decode.string)
contentDecoder = Decode.field "content" Decode.string
commentsDecoder = Decode.maybe (Decode.field "comments" (Decode.list Decode.string))
-- amount_of_commentsDecoder = Decode.field "amount-of-comments" Decode.int                  
titleDecoder = Decode.field "title" Decode.string
pre_post_idDecoder = Decode.maybe (Decode.field "prev-post-id"  Decode.int)
idDecoder = Decode.field "id" Decode.int
versionsDecoder = Decode.field "versions" (Decode.list Decode.int)
versionDecoder = Decode.maybe (Decode.field "version" Decode.int)
-- next_post_idDecoder = Decode.field "next-post-id" (Decode.maybe Decode.int)
created_atDecoder = Decode.field "created_at" (Decode.maybe Extra.datetime)
creator_Decoder = Decode.field "creator" creatorDecoder                    

-- |> == clojure's ->>
articleDecoder : Decoder Article                    
articleDecoder =
    Decode.succeed Article
        |> decodeApply creator_Decoder
        |> decodeApply tagsDecoder
        |> decodeApply contentDecoder
        |> decodeApply commentsDecoder
        |> decodeApply titleDecoder
        |> decodeApply pre_post_idDecoder
        |> decodeApply idDecoder
        |> decodeApply versionsDecoder
        |> decodeApply versionDecoder
        |> decodeApply created_atDecoder

type alias Title =
    { title : String
    , id : Int
    , year : Int
    , month: String
    , tags: List String
    }
    

sidebarTitleDecoder =
    Decode.succeed Title
        |> decodeApply (Decode.field "Title" Decode.string)
        |> decodeApply (Decode.field "Id" Decode.int)
        |> decodeApply (Decode.field "Year" Decode.int)
        |> decodeApply (Decode.field "Month" Decode.string)
        |> decodeApply (Decode.field "Tags" (Decode.list Decode.string))

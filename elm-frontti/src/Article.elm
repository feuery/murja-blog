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

type alias Article =
    { creator : Creator
    , tags : List String
    , content : String
    -- TODO make a comment type
    , comments : List String
    -- , amount_of_comments : Int
    , title : String
    -- , pre_post_id : Maybe Int
    -- , id : Int
    , versions: List Int
    , version : Int
    -- , next_post_id: Maybe Int
    , created_at: Maybe Time.Posix
    }
                  

tagsDecoder = Decode.field "tags" (Decode.list Decode.string)
contentDecoder = Decode.field "content" Decode.string
commentsDecoder = Decode.field "comments" (Decode.list Decode.string)
-- amount_of_commentsDecoder = Decode.field "amount-of-comments" Decode.int                  
titleDecoder = Decode.field "title" Decode.string
-- pre_post_idDecoder = Decode.field "prev-post-id" (Decode.maybe Decode.int)
-- idDecoder = Decode.field "id" Decode.int
versionsDecoder = Decode.field "versions" (Decode.list Decode.int)
versionDecoder = Decode.field "version" Decode.int
-- next_post_idDecoder = Decode.field "next-post-id" (Decode.maybe Decode.int)
created_atDecoder = Decode.field "created_at" (Decode.maybe Extra.datetime)

articleDecoder : Decoder Article                    
articleDecoder =
    Decode.map8 Article
        Creator.creatorDecoder
        tagsDecoder
        contentDecoder
        commentsDecoder
        titleDecoder
        versionsDecoder
        versionDecoder
        created_atDecoder

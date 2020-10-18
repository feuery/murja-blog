module Article exposing (..)

import DateTime exposing (DateTime)
import Html exposing (..)

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

import Creator exposing (Creator)

type alias Article =
    { creator : Creator
    , tags : List String
    , content : String
    -- TODO make a comment type
    , comments : List String
    , amount_of_comments : Int
    , title : String
    , pre_post_id : Maybe Int
    , id : Int
    , versions: List Int
    , version : Int
    , next_post_id: Maybe Int
    , created_at: Maybe DateTime
    }
                  

view : Article -> Html msg
view model =
    div []
        [ h1 [] [text model.title]
        , div [] [text model.content]]

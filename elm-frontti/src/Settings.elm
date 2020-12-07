-- {
--   "time-format": "dd.MM.yyyy HH:mm",
--   "blog-title": "Murja.dev @ roland",
--   "recent-post-count": 6,
--   "xss-filter-posts?": false
-- }


module Settings exposing (..)

import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Decode.Extra as Extra

import Article

type alias Settings =
    { time_format : String
    , blog_title : String
    , recent_post_count : Int
    , xss_filter_posts : Bool

    , titles : Maybe (List Article.Title)     --for reasons fucking unknown, growing Main.Model beoynd 2 fields breaks everything. 
    }

settingsDecoder = Decode.map5 Settings
                  (Decode.field "time-format" Decode.string)
                  (Decode.field "blog-title" Decode.string)
                  (Decode.field "recent-post-count" Decode.int)
                  (Decode.field "xss-filter-posts?" Decode.bool)
                  (Decode.maybe (Decode.list (Decode.field "does-not-exist" Article.sidebarTitleDecoder)))
                     

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

type alias Settings =
    { time_format : String
    , blog_title : String
    , recent_post_count : Int
    , xss_filter_posts : Bool
    }

settingsDecoder = Decode.map4 Settings
                  (Decode.field "time-format" Decode.string)
                  (Decode.field "blog-title" Decode.string)
                  (Decode.field "recent-post-count" Decode.int)
                  (Decode.field "xss-filter-posts?" Decode.bool)
                      

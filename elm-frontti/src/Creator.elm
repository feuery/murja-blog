module Creator exposing (..)

import Json.Encode as Json exposing (..)
import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (required)

type alias Creator =
    { username : String
    , nickname : String
    , img_location : String}

usernameDecoder = Decode.field "username" Decode.string
nicknameDecoder = Decode.field "nickname" Decode.string
img_locationDecoder = Decode.field "img_location" Decode.string

creatorDecoder = Decode.map3 Creator usernameDecoder nicknameDecoder img_locationDecoder                      

-- encoder

encode : Creator -> Json.Value
encode creator =
    object
        [ ( "username", string creator.username)
        , ( "nickname", string creator.nickname)
        , ( "img_location", string creator.img_location)
        ]

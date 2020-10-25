module Creator exposing (Creator, creatorDecoder)

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

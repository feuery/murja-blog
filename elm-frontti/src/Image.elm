module Image exposing (..)

import Json.Encode as Json exposing (..)
import Json.Encode.Extra exposing (..)
import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Decode.Extra as Extra

import UUID exposing (UUID)

import Article exposing (decodeApply)

type alias Image =
    { id: UUID
    , name: String }

type alias PostImageResponse =
    { id: UUID }

encode img =
    object
        [ ("id", UUID.toValue img.id)
        , ("name", string img.name) ]

idDecoder = Decode.field "id" UUID.jsonDecoder
nameDecoder = Decode.field "name" Decode.string

imageDecoder =
    Decode.succeed Image
        |> decodeApply idDecoder
        |> decodeApply nameDecoder
imageResponseDecoder = Decode.succeed PostImageResponse
                       |> decodeApply idDecoder

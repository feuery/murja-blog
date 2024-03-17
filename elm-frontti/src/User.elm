module User exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Message exposing (..)
import Article exposing (decodeApply)
import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Decode.Extra as Extra
import Json.Encode as Json


    -- {
    --       "nickname": "Feuer",
    --             "img_location": "https://feuerx.net/etc/feuer.jpeg",
    --             "userid": 1,
    --             "primary-group-name": "Admins",
    --             "permissions": [
    --                  "edit-self",
    --                          "comment-post",
    --                          "edit-user",
    --                          "create-comment",
    --                          "edit-post",
    --                          "delete-post",
    --                          "create-post",
    --                          "create-page",
    --                          "delete-comment",
    --                          "delete-user",
    --                          "can-import",
    --                          "edit-comment"
    --                        ]


nicknameDecoder = Decode.field "nickname" Decode.string
imgDecoder = Decode.field "img_location" Decode.string
group_name_decoder = Decode.field "primary-group-name" Decode.string
permissionsDecoder = Decode.field "permissions" (Decode.list Decode.string)
usernameDecoder = Decode.field "username" Decode.string                  
                     
-- |> == clojure's ->>
userDecoder : Decoder LoginUser
userDecoder =
    Decode.succeed LoginUser
        |> decodeApply nicknameDecoder
        |> decodeApply usernameDecoder
        |> decodeApply imgDecoder
        |> decodeApply group_name_decoder
        |> decodeApply permissionsDecoder
    
stateToText state =
    case state of
        LoggedIn _ -> "LoggedIn"
        LoggingIn _ _ -> "LoggingIn"
        LoggedOut -> "LoggedOut"
        LoginFailed -> "LoginFailed"
           
loginView loginstate =
    let actual_view = [label [for "username"] [text "Username"],
                       input [name "username", id "username", attribute "data-testid" "username-input-field", onInput ChangeUsername, onFocus LoginFocus ] [],
                       label [for "password"] [text "Password"],
                       input [name "password", attribute "data-testid" "password-input-field", id "password", type_ "password", onInput ChangePassword ] []
                           -- , label [] [text ("Loginstate: " ++ stateToText loginstate)]
                      ] in
    div [] (case loginstate of
                                  LoggedIn usr ->
                                      [p [attribute "data-testid" "welcome-user-label"] [text ("Welcome, " ++ usr.nickname)]]
                                  LoggingIn username password ->
                                      (List.concat [actual_view,
                                                    [button [attribute "data-testid" "dologin", onClick DoLogIn] [text "Login!"]]])
                                  LoggedOut ->
                                      actual_view
                                  LoginFailed ->
                                      (List.concat [actual_view,
                                                    [button [onClick DoLogIn] [text "Login!"],
                                                     div [attribute "data-testid" "loginfailed"] [text "Login failed! Check username and password!"]]]))

user_avatar creator = img [class "user_avatar", src creator.img_location] []

type alias UserLoggingIn =
    { username : String
    , password : String}

encodeLoggingIn user =
    Json.object
        [ ("username", Json.string user.username)
        , ("password", Json.string user.password)]

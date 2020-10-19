module Page exposing (..)


import Http
import Html exposing (Html, text, pre)
import Article as A

type alias Page =
    { last_page: Bool
    , posts: List A.Article}

type Status a
    = Loading
    | Loaded a
    | Failed
      
init _ =
    ( Loading
    , Http.get
        { url = "/api/posts/page/1/page-size/10"
        , expect = Http.expectString GotText
        })

type Msg
    = GotText (Result Http.Error String)


-- update : Msg -> Page -> (Page, Cmd Msg)      
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok text ->
                    (Loaded text, Cmd.none)

                Err _ ->
                    (Failed, Cmd.none)

-- subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none




view model =
    case model of
        Failed ->
            text "Failure loading page"
        Loading ->
            text "Loading page..."
        Loaded result_json ->
            pre [] [ text result_json ]
                

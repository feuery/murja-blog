module RouteParser exposing (..)

import Url
import Url.Parser exposing (..)
import String exposing (fromInt)
-- http://localhost:3000/blog/post/edit/21
type Route
    = Page Int
    | Post Int
    | PostEditor Int
    | Home
    | NotFound

routeParser =
    oneOf
        [ map Page (s "blog" </> (s "page" </> int))
        , map Home Url.Parser.top
        , map Home (s "blog")
        , map Post (s "blog" </> (s "post" </> int))
        , map PostEditor (s "blog" </> (s "post" </> (s "edit" </> int)))]

url_to_route url =
            Maybe.withDefault NotFound (parse routeParser url)


-- for debug reasons
route_to_string route =
    case route of
        Page page ->
            ("Page " ++ (fromInt page))
        Post post ->
            ("Post " ++ (fromInt post))
        Home -> "Home"
        PostEditor id -> "Editing post " ++ String.fromInt id
        NotFound -> "Parsing failed"

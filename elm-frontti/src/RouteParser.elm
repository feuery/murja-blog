module RouteParser exposing (..)

import Url
import Url.Parser exposing (..)
import String exposing (fromInt)

type Route
    = Page Int
    | Post Int
    | Home
    | NotFound

routeParser =
    oneOf
        [ map Page (s "blog" </> (s "page" </> int))
        , map Home Url.Parser.top
        , map Home (s "blog")
        , map Post (s "blog" </> (s "post" </> int))]

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
        NotFound -> "Parsing failed"

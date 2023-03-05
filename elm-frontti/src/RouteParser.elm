module RouteParser exposing (..)

import Url
import Url.Parser exposing (..)
import String exposing (fromInt)
-- http://localhost:3000/blog/post/edit/21
type Route
    = Page Int
    | Post Int
    | NewPost 
    | PostAdmin
    | MediaManager
    | PostEditor Int
    | TaggedPosts String
    | Home
    | NotFound

routeParser =
    oneOf
        [ map Page (s "blog" </> (s "page" </> int))
        , map Home Url.Parser.top
        , map Home (s "blog")
        , map Post (s "blog" </> (s "post" </> int))
        , map PostEditor (s "blog" </> (s "post" </> (s "edit" </> int)))
        , map MediaManager (s "blog" </> (s "mediamanager"))
        , map NewPost (s "blog" </> (s "new_post"))
        , map TaggedPosts (s "blog" </> (s "tags" </> string))
        , map PostAdmin (s "blog" </> (s "postadmin"))]

url_to_route url =
            Maybe.withDefault NotFound (parse routeParser url)

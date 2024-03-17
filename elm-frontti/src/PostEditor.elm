module PostEditor exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
    

import Http

import Article_view
import Ajax_cmds exposing (..)
import Creator as C
import Page as P
import Message exposing (..)
import ImageSelector exposing (imageSelector)
import Button exposing (murja_button)

import File exposing (File)
import File.Select as Select

dropDecoder : D.Decoder Msg
dropDecoder =
  D.at ["dataTransfer","files"] (D.oneOrMore GotFiles File.decoder)


hijackOn : String -> D.Decoder msg -> Attribute msg
hijackOn event decoder =
  preventDefaultOn event (D.map hijack decoder)


hijack : msg -> (msg, Bool)
hijack msg =
  (msg, True)
      

optionize tag = option [value tag] [text tag]

tagView post selectedTag = div [class "tagview"]
                           [ select [ multiple True
                                    , class "tag-select"
                                    , id "tag-select"
                                    , onInput SelectTag
                                    , attribute "data-tags" (String.join "," post.tags)] (List.map optionize post.tags)
                           , murja_button [ onClick (PromptTag "New tag? ")
                                          , id "new-tag-btn"]
                                 [ text "Add tag"]
                           , murja_button [ onClick (DropTag selectedTag)
                                          , attribute "data-testid" "remove-tag"]
                               [text "Remove selected tag"]]

third_column = div [class "tagview" ]
               [ murja_button [ onClick ClearLocalStorage ] [ text "Clear post in the editor" ] ]

editor params =
    node "ace-editor"
    (  params
    ++ [ attribute "theme" "ace/theme/monokai"
       , attribute "mode" "ace/mode/html"])
    []

filesDecoder : D.Decoder (List File)
filesDecoder =
  D.at ["target","files"] (D.list File.decoder)
      
postEditor post tag showImageModal loadedImages draggingImages editorSettings app_settings tz loginState
    = [ div [ id "editor-buttons"]
            [ input [ name "title"
                    , id "editor-post-title"
                    , value post.title
                    , onInput ChangeTitle] []
            , murja_button [ id "editor-post-save"
                           , onClick (SavePost post) ] [text "Save version"]
            , label [ for "file-pictures-input"
                    , class "murja-button"] [ text "Add pictures from device"]
            , input [ type_ "file"
                    , multiple False
                    , style "display" "none"
                    , id "file-pictures-input"
                    , on "change" (D.map GotInputFiles filesDecoder)] []
            , murja_button [ id "image-insert-btn"
                           , onClick GetListOfImages]
                  [text "Insert image"]
            , label [for "show-preview-cb"]
                [text "Show article preview"]
            , input [ type_ "checkbox"
                    , id "show-preview-cb"
                    , checked editorSettings.show_preview
                    , onClick ToggleArticlePreview] []]
            
      , tagView post tag
      , third_column
      , if showImageModal then imageSelector loadedImages else div [] []

      , if editorSettings.show_preview then
            case loginState of
                LoggedIn user ->
                    Article_view.articleView app_settings loginState tz post
                _ -> div [] [text "You're not logged in"]
                        
        else editor [ id "editor-post-content"
                    , style "background-color" (if draggingImages then "#880088" else "")
                    , hijackOn "dragenter" (D.succeed EditorDragEnter)
                    , hijackOn "dragend" (D.succeed EditorDragLeave)
                    , hijackOn "dragover" (D.succeed EditorDragEnter)
                    , hijackOn "dragleave" (D.succeed EditorDragLeave)
                    , hijackOn "drop" dropDecoder
                    , hijackOn "ready" (D.succeed (RunAce post.content))]]

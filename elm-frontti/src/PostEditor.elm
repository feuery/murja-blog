module PostEditor exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
    

import Http

import Article
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
                        , onInput SelectTag] (List.map optionize post.tags)
               , murja_button [ onClick (PromptTag "New tag? ")
                              , id "new-tag-btn"]
                    [ text "Add tag"]
               , murja_button [ onClick (DropTag selectedTag) ]
                    [text "Remove selected tag"]
               ]

editor params =
    node "ace-editor"
    (  params
    ++ [ attribute "theme" "ace/theme/monokai"
       , attribute "mode" "ace/mode/html"])
    []

filesDecoder : D.Decoder (List File)
filesDecoder =
  D.at ["target","files"] (D.list File.decoder)
      
postEditor post tag showImageModal loadedImages draggingImages
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
                  [text "Insert image"]]
            
      , tagView post tag
      , if showImageModal then imageSelector loadedImages else div [] []
      , editor [ id "editor-post-content"
            , style "background-color" (if draggingImages then "#880088" else "")
            , hijackOn "dragenter" (D.succeed EditorDragEnter)
            , hijackOn "dragend" (D.succeed EditorDragLeave)
            , hijackOn "dragover" (D.succeed EditorDragEnter)
            , hijackOn "dragleave" (D.succeed EditorDragLeave)
            , hijackOn "drop" dropDecoder
            , hijackOn "ready" (D.succeed (RunAce post.content))
            ]]
    
    

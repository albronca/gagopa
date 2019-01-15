module Song exposing (Song, decoder, listDecoder)

import Json.Decode as Decode exposing (Decoder, field, list, maybe, string)
import String.Extra exposing (ellipsis, toTitleCase)


type alias Song =
    { artist : String
    , code : String
    , key : Maybe String
    , title : String
    }


decoder : Decoder Song
decoder =
    Decode.map4 Song
        (field "artist" string)
        (field "code" string)
        (maybe (field "key" string))
        (field "title" string)
        |> Decode.map titleCaseRelevantFields


listDecoder : Decoder (List Song)
listDecoder =
    list decoder


titleCaseRelevantFields : Song -> Song
titleCaseRelevantFields song =
    { song
        | artist = song.artist |> titleCase
        , title = song.title |> titleCase
    }


titleCase : String -> String
titleCase =
    String.toLower >> toTitleCase

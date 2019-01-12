module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Decode exposing (..)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { query : String
    , results : List SongData
    }


type alias SongData =
    { code : String
    , title : String
    , artist : String
    }


initialModel : Model
initialModel =
    { query = ""
    , results = []
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = QueryChange String
    | StartSearch
    | ApiResponse (Result Http.Error (List SongData))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        QueryChange newQuery ->
            ( { model | query = newQuery }, Cmd.none )

        StartSearch ->
            ( model, getJson model.query )

        ApiResponse response ->
            case response of
                Ok newResults ->
                    ( { model | results = newResults }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    layoutWith { options = layoutOptions }
        [ Background.color black
        , Font.family
            [ Font.external
                { name = "Roboto Mono"
                , url = "https://fonts.googleapis.com/css?family=Roboto+Mono"
                }
            , Font.monospace
            ]
        ]
    <|
        column
            [ Font.color pink
            , centerX
            , width <| maximum 600 <| fill
            , height fill
            , spacing 40
            ]
            [ header
            , searchBox model.query
            , resultsList model.results
            ]


layoutOptions : List Option
layoutOptions =
    [ focusStyle
        { borderColor = Nothing
        , backgroundColor = Just purple
        , shadow = Nothing
        }
    ]


header : Element Msg
header =
    text "ガゴパ"
        |> el
            [ Font.size 60
            , Font.family
                [ Font.external
                    { name = "Nico Moji"
                    , url = "https://fonts.googleapis.com/earlyaccess/nicomoji.css"
                    }
                ]
            , Font.center
            , width fill
            , padding 40
            ]


searchBox : String -> Element Msg
searchBox inputText =
    row [ centerX, width <| maximum 600 <| fill ]
        [ Input.text
            [ width <| fillPortion 3
            , Background.color black
            , Border.color pink
            , Border.roundEach
                { topLeft = 5
                , topRight = 0
                , bottomLeft = 5
                , bottomRight = 0
                }
            ]
            { onChange = QueryChange
            , text = inputText
            , placeholder = searchInputPlaceholder
            , label = Input.labelHidden "Search"
            }
        , Input.button
            [ Font.center
            , width <| fillPortion 1
            , height fill
            , Border.width 1
            , Border.roundEach
                { topLeft = 0
                , topRight = 5
                , bottomLeft = 0
                , bottomRight = 5
                }
            ]
            { onPress = Just StartSearch
            , label = text "Search"
            }
        ]


searchInputPlaceholder : Maybe (Input.Placeholder Msg)
searchInputPlaceholder =
    text "Enter a song or artist name"
        |> Input.placeholder [ Font.color gray ]
        |> Just


resultsList : List SongData -> Element Msg
resultsList results =
    Element.table
        [ width fill
        , Border.width 1
        , Border.rounded 5
        , height fill
        , height shrink
        ]
        { data = results
        , columns =
            [ { header = resultsListHeader "Song Title"
              , width = fill
              , view = .title >> resultsListCell [ Font.alignLeft ]
              }
            , { header = resultsListHeader "Artist Name"
              , width = fill
              , view = .artist >> resultsListCell [ Border.widthXY 1 0, Border.dotted ]
              }
            , { header = resultsListHeader "Code"
              , width = fill
              , view = .code >> resultsListCell []
              }
            ]
        }


resultsListHeader : String -> Element Msg
resultsListHeader headerText =
    text headerText
        |> el
            [ Font.bold
            , Font.center
            , padding 10
            , Border.widthEach
                { top = 0
                , right = 0
                , bottom = 1
                , left = 0
                }
            ]


resultsListCell : List (Attribute Msg) -> String -> Element Msg
resultsListCell attributes cellText =
    text cellText
        |> el ([ padding 10, Font.center ] ++ attributes)



-- COLORS


black : Color
black =
    rgb255 17 17 17


purple : Color
purple =
    rgb255 41 21 40


gray : Color
gray =
    rgb255 58 62 59


white : Color
white =
    rgb255 240 239 244


pink : Color
pink =
    rgb255 158 130 156



-- API


getJson : String -> Cmd Msg
getJson query =
    let
        url =
            "http://gagopa.herokuapp.com/songs?query=" ++ query
    in
    Http.send ApiResponse <|
        Http.get url songDataListDecoder


songDataListDecoder : Decoder (List SongData)
songDataListDecoder =
    list songDataDecoder


songDataDecoder : Decoder SongData
songDataDecoder =
    map3 SongData
        (field "code" string)
        (field "title" string)
        (field "artist" string)

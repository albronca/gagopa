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
import String.Extra exposing (ellipsis)
import Url.Builder


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
    , language : Language
    , showResults : Bool
    , results : List SongData
    }


type Language
    = English
    | Spanish


type alias SongData =
    { code : String
    , title : String
    , artist : String
    }


languageToString : Language -> String
languageToString language =
    case language of
        English ->
            "English"

        Spanish ->
            "Spanish"


initialModel : Model
initialModel =
    { query = ""
    , language = English
    , showResults = False
    , results = []
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = ApiResponse (Result Http.Error (List SongData))
    | QueryChange String
    | SelectLanguage Language
    | StartSearch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ApiResponse response ->
            case response of
                Ok newResults ->
                    ( { model | showResults = True, results = newResults }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        QueryChange newQuery ->
            ( { model | query = newQuery }, Cmd.none )

        SelectLanguage newLanguage ->
            ( { model | language = newLanguage }, Cmd.none )

        StartSearch ->
            ( model, getJson model )



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
            , searchBox model
            , if model.showResults then
                resultsList model.results

              else
                none
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


languageSelect : Language -> Element Msg
languageSelect language =
    Input.radioRow [ Font.size 16 ]
        { onChange = SelectLanguage
        , options = languageOptions
        , selected = Just language
        , label = Input.labelHidden "Language"
        }


languageOptions : List (Input.Option Language Msg)
languageOptions =
    [ languageOption
        [ padding 10
        , Border.widthEach
            { top = 1
            , right = 1
            , bottom = 0
            , left = 1
            }
        , Border.roundEach
            { topLeft = 5
            , topRight = 0
            , bottomLeft = 0
            , bottomRight = 0
            }
        ]
        English
    , languageOption
        [ padding 10
        , Border.widthEach
            { top = 1
            , right = 1
            , bottom = 0
            , left = 0
            }
        , Border.roundEach
            { topLeft = 0
            , topRight = 5
            , bottomLeft = 0
            , bottomRight = 0
            }
        ]
        Spanish
    ]


languageOption : List (Attribute Msg) -> Language -> Input.Option Language Msg
languageOption attributes language =
    Input.optionWith language
        (\optionState ->
            case optionState of
                Input.Idle ->
                    el attributes <| text <| languageToString language

                Input.Focused ->
                    el (attributes ++ [ Background.color purple ]) <|
                        text <|
                            languageToString language

                Input.Selected ->
                    el (attributes ++ [ Background.color purple ]) <|
                        text <|
                            languageToString language
        )


searchBox : Model -> Element Msg
searchBox model =
    column [ width fill ]
        [ languageSelect model.language
        , row [ centerX, width <| maximum 600 <| fill ]
            [ Input.text
                [ width <| fillPortion 3
                , Background.color black
                , Border.color pink
                , Border.roundEach
                    { topLeft = 0
                    , topRight = 0
                    , bottomLeft = 5
                    , bottomRight = 0
                    }
                ]
                { onChange = QueryChange
                , text = model.query
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
        ]


searchInputPlaceholder : Maybe (Input.Placeholder Msg)
searchInputPlaceholder =
    text "Enter a song or artist name"
        |> Input.placeholder [ Font.color gray ]
        |> Just


resultsList : List SongData -> Element Msg
resultsList results =
    if List.isEmpty results then
        el [ centerX ] <| text "No results found"

    else
        column [ width fill ]
            [ Element.table
                [ width fill
                , Border.width 1
                , Border.rounded 5
                , clipY
                , scrollbarY
                , height <| maximum 350 <| fill
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
            , el [ centerX ] <| text "1|2|3"
            ]


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
    ellipsis 23 cellText
        |> text
        |> el ([ padding 10, Font.center, Font.size 16 ] ++ attributes)



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


getJson : Model -> Cmd Msg
getJson model =
    let
        url =
            Url.Builder.crossOrigin "https://gagopa.herokuapp.com"
                [ "songs" ]
                [ Url.Builder.string "query" model.query
                , Url.Builder.string "language" (languageToString model.language)
                ]
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

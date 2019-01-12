port module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Decode as Decode exposing (Decoder, Error, field, maybe, string)
import Json.Encode as Encode
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
    , authForm : Maybe AuthForm
    , results : List SongData
    , userSongs : List SongData
    , email : String
    , password : String
    , uid : Maybe String
    }


type Language
    = English
    | Spanish


type AuthForm
    = SignIn
    | SignUp


type alias SongData =
    { key : Maybe String
    , code : String
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
    , authForm = Nothing
    , results = []
    , userSongs = []
    , email = ""
    , password = ""
    , uid = Nothing
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = AddSong SongData
    | ApiResponse (Result Http.Error (List SongData))
    | ChangeEmail String
    | ChangePassword String
    | ReceiveUid (Maybe String)
    | CreateUser
    | QueryChange String
    | RemoveSong String
    | SelectLanguage Language
    | SignInUser
    | SignOut
    | SongAdded (Result Error SongData)
    | SongRemoved String
    | StartSearch
    | ShowAuthForm AuthForm
    | ReceiveUserSongs (Result Error (List SongData))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddSong songData ->
            case model.uid of
                Just uid ->
                    ( model
                    , addSong
                        { code = songData.code
                        , title = songData.title
                        , artist = songData.artist
                        , uid = uid
                        }
                    )

                Nothing ->
                    ( model, Cmd.none )

        ApiResponse response ->
            case response of
                Ok newResults ->
                    ( { model | showResults = True, results = newResults }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        ChangeEmail newEmail ->
            ( { model | email = newEmail }, Cmd.none )

        ChangePassword newPassword ->
            ( { model | password = newPassword }, Cmd.none )

        ReceiveUid newUid ->
            ( { model | uid = newUid }, Cmd.none )

        CreateUser ->
            ( model, createUser ( model.email, model.password ) )

        QueryChange newQuery ->
            ( { model | query = newQuery }, Cmd.none )

        RemoveSong key ->
            case model.uid of
                Just uid ->
                    ( model, removeSong ( uid, key ) )

                Nothing ->
                    ( model, Cmd.none )

        SelectLanguage newLanguage ->
            ( { model | language = newLanguage }, Cmd.none )

        SignInUser ->
            ( model, signInUser ( model.email, model.password ) )

        SignOut ->
            ( model, signOut () )

        SongAdded result ->
            case result of
                Ok songData ->
                    let
                        userSongs =
                            model.userSongs ++ [ songData ]
                    in
                    ( { model | userSongs = userSongs }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        SongRemoved key ->
            let
                userSongs =
                    model.userSongs
                        |> List.filter (\song -> song.key /= Just key)
            in
            ( { model | userSongs = userSongs }, Cmd.none )

        StartSearch ->
            ( model, getJson model )

        ShowAuthForm formType ->
            ( { model | authForm = Just formType }, Cmd.none )

        ReceiveUserSongs result ->
            case result of
                Ok userSongs ->
                    ( { model | userSongs = userSongs }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveNewUid ReceiveUid
        , (Decode.decodeValue songDataListDecoder >> ReceiveUserSongs)
            |> receiveUserSongs
        , songRemoved SongRemoved
        , (Decode.decodeValue songDataDecoder >> SongAdded)
            |> songAdded
        ]



-- VIEW


view : Model -> Html Msg
view model =
    layoutWith { options = layoutOptions }
        [ Background.color black
        , Font.family
            [ Font.typeface "Roboto Mono"
            , Font.monospace
            ]
        , inFront <| authWrapper model
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
            , if not <| List.isEmpty model.userSongs then
                resultsList model.userSongs

              else
                none
            ]


layoutOptions : List Option
layoutOptions =
    [ focusStyle
        { borderColor = Nothing
        , backgroundColor = Nothing
        , shadow = Nothing
        }
    ]


header : Element Msg
header =
    text "ガゴパ"
        |> el
            [ Font.size 60
            , Font.family [ Font.typeface "Nico Moji" ]
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
            [ Input.search
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
                      , view =
                            \songData ->
                                resultsListCell
                                    [ Font.alignLeft
                                    , inFront <| addRemoveButton songData
                                    ]
                                    songData.title
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
            ]


addRemoveButton : SongData -> Element Msg
addRemoveButton songData =
    let
        ( onPress, labelText ) =
            case songData.key of
                Just key ->
                    ( Just (RemoveSong key), "-" )

                Nothing ->
                    ( Just (AddSong songData), "+" )
    in
    Input.button
        [ Font.color pink ]
        { onPress = onPress, label = text labelText }


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


authWrapper : Model -> Element Msg
authWrapper model =
    column [ Font.color pink, Font.size 12 ]
        (case model.uid of
            Just uid ->
                [ Input.button [ padding 10 ]
                    { onPress = Just SignOut
                    , label = text "Sign Out"
                    }
                ]

            Nothing ->
                [ row []
                    [ Input.button [ padding 10 ]
                        { onPress = Just (ShowAuthForm SignIn)
                        , label = text "Sign In ⌄"
                        }
                    , Input.button [ padding 10 ]
                        { onPress = Just (ShowAuthForm SignUp)
                        , label = text "Sign Up ⌄"
                        }
                    ]
                , case model.authForm of
                    Just formType ->
                        authForm formType model

                    Nothing ->
                        none
                ]
        )


authForm : AuthForm -> Model -> Element Msg
authForm formType model =
    let
        ( passwordField, onSubmit, labelText ) =
            case formType of
                SignIn ->
                    ( Input.currentPassword, Just SignInUser, "Sign In" )

                SignUp ->
                    ( Input.newPassword, Just CreateUser, "Sign Up" )
    in
    column [ Background.color transparentPurple, padding 20, spacing 10 ]
        [ Input.email []
            { onChange = ChangeEmail
            , text = model.email
            , placeholder = Nothing
            , label = Input.labelAbove [] <| text "email"
            }
        , passwordField []
            { onChange = ChangePassword
            , text = model.password
            , placeholder = Nothing
            , show = False
            , label = Input.labelAbove [] <| text "password"
            }
        , Input.button []
            { onPress = onSubmit
            , label = text labelText
            }
        ]



-- COLORS


black : Color
black =
    rgb255 17 17 17


purple : Color
purple =
    rgb255 41 21 40


transparentPurple : Color
transparentPurple =
    rgba255 41 21 40 0.8


gray : Color
gray =
    rgb255 58 62 59


white : Color
white =
    rgb255 240 239 244


pink : Color
pink =
    rgb255 158 130 156



-- PORTS


port signInUser : ( String, String ) -> Cmd msg


port createUser : ( String, String ) -> Cmd msg


port signOut : () -> Cmd msg


port addSong : { uid : String, code : String, title : String, artist : String } -> Cmd msg


port removeSong : ( String, String ) -> Cmd msg


port receiveNewUid : (Maybe String -> msg) -> Sub msg


port receiveUserSongs : (Encode.Value -> msg) -> Sub msg


port songRemoved : (String -> msg) -> Sub msg


port songAdded : (Encode.Value -> msg) -> Sub msg



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
    Decode.list songDataDecoder


songDataDecoder : Decoder SongData
songDataDecoder =
    Decode.map4 SongData
        (maybe (field "key" string))
        (field "code" string)
        (field "title" string)
        (field "artist" string)

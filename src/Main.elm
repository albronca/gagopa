port module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes exposing (style, title)
import Http
import Json.Decode as Decode exposing (Decoder, Error, field, maybe, string)
import Json.Encode as Encode
import String.Extra exposing (ellipsis)
import Toasty
import Toasty.Defaults exposing (..)
import Url.Builder


main : Program () Model Msg
main =
    Browser.document
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
    , toasties : Toasty.Stack Toast
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
    , toasties = Toasty.initialState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


languageToString : Language -> String
languageToString language =
    case language of
        English ->
            "English"

        Spanish ->
            "Spanish"


toastyConfig : Toasty.Config Msg
toastyConfig =
    config
        |> Toasty.containerAttrs
            [ style "max-width" "300px"
            , style "position" "fixed"
            , style "right" "0"
            , style "top" "0"
            , style "font-family" "Roboto Mono"
            , style "list-style" "none"
            ]



-- UPDATE


type Msg
    = AddSong SongData
    | ApiResponse (Result Http.Error (List SongData))
    | ChangeEmail String
    | ChangePassword String
    | CloseAuthForm
    | ReceiveUid (Maybe String)
    | CreateUser
    | QueryChange String
    | ReceiveError String
    | ReceiveUserSongs (Result Error (List SongData))
    | RemoveSong String
    | SelectLanguage Language
    | SignInUser
    | SignOut
    | SongAdded (Result Error SongData)
    | SongRemoved String
    | StartSearch
    | ShowAuthForm AuthForm
    | ToastyMsg (Toasty.Msg Toast)


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
                        |> Toasty.addToast toastyConfig
                            ToastyMsg
                            (Warning
                                "Dammit, Janet!"
                                "Please sign in to add songs to your wishlist."
                            )

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

        CloseAuthForm ->
            ( { model | authForm = Nothing }, Cmd.none )

        CreateUser ->
            ( model, createUser ( model.email, model.password ) )

        QueryChange newQuery ->
            ( { model | query = newQuery }, Cmd.none )

        ReceiveError message ->
            ( model, Cmd.none )
                |> Toasty.addToast toastyConfig ToastyMsg (Error "Bismillah NO!" message)

        ReceiveUid newUid ->
            case newUid of
                Just _ ->
                    ( { model | uid = newUid, email = "", password = "" }, Cmd.none )
                        |> Toasty.addToast toastyConfig
                            ToastyMsg
                            (Success
                                "Heathcliff, it's me, I'm Cathy!"
                                "You've successfully signed in."
                            )

                Nothing ->
                    ( { model | uid = newUid }, Cmd.none )

        ReceiveUserSongs result ->
            case result of
                Ok userSongs ->
                    ( { model | userSongs = userSongs }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

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

        ToastyMsg subMsg ->
            Toasty.update toastyConfig ToastyMsg subMsg model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveNewUid ReceiveUid
        , receiveError ReceiveError
        , (Decode.decodeValue songDataListDecoder >> ReceiveUserSongs)
            |> receiveUserSongs
        , songRemoved SongRemoved
        , (Decode.decodeValue songDataDecoder >> SongAdded)
            |> songAdded
        ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "gagopa.club"
    , body =
        [ layoutWith { options = layoutOptions }
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
                , width <| maximum 1000 <| fill
                , height fill
                , spacing 20
                ]
                [ header
                , languageSelect model.language
                , searchBox model.query
                , row [ width fill, spacing 10 ]
                    [ if model.showResults then
                        column [ alignTop, width <| fillPortion 3 ]
                            [ el [ centerX, Font.size 12, padding 10 ] <| text "Search Results"
                            , songList { charLimit = 30, background = black, font = pink } model.results
                            ]

                      else
                        none
                    , if not <| List.isEmpty model.userSongs then
                        let
                            charLimit =
                                if not <| List.isEmpty model.results then
                                    15

                                else
                                    30
                        in
                        column
                            [ alignTop, width <| fillPortion 1 ]
                            [ el [ centerX, Font.size 12, padding 10 ] <| text "Wish List"
                            , songList { charLimit = charLimit, background = purple, font = white }
                                model.userSongs
                            ]

                      else
                        none
                    ]
                ]
        , Toasty.view toastyConfig Toasty.Defaults.view ToastyMsg model.toasties
        ]
    }


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
            , paddingEach
                { top = 40
                , right = 0
                , bottom = 0
                , left = 0
                }
            ]


languageSelect : Language -> Element Msg
languageSelect language =
    row [ Font.size 16, centerX ]
        [ Input.button
            [ padding 10
            , Border.width 1
            , Border.roundEach
                { topLeft = 5
                , topRight = 0
                , bottomLeft = 5
                , bottomRight = 0
                }
            , Background.color
                (if language == English then
                    purple

                 else
                    black
                )
            ]
            { onPress = Just (SelectLanguage English)
            , label = text "English"
            }
        , Input.button
            [ padding 10
            , Border.widthEach
                { top = 1
                , right = 1
                , bottom = 1
                , left = 0
                }
            , Border.roundEach
                { topLeft = 0
                , topRight = 5
                , bottomLeft = 0
                , bottomRight = 5
                }
            , Background.color
                (if language == Spanish then
                    purple

                 else
                    black
                )
            ]
            { onPress = Just (SelectLanguage Spanish)
            , label = text "Spanish"
            }
        ]


searchBox : String -> Element Msg
searchBox query =
    row [ width <| maximum 600 <| fill, centerX ]
        [ Input.search
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
            , text = query
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
        |> Input.placeholder [ Font.color gray, clip ]
        |> Just


type alias SongListOptions =
    { charLimit : Int
    , background : Color
    , font : Color
    }


songList : SongListOptions -> List SongData -> Element Msg
songList options results =
    if List.isEmpty results then
        el [ centerX ] <| text "No results found"

    else
        column [ width <| maximum 600 <| fill, centerX ]
            [ Element.table
                [ width fill
                , Border.width 1
                , Border.rounded 5
                , clipY
                , scrollbarY
                , height <| maximum 350 <| fill
                , Background.color options.background
                , Font.color options.font
                ]
                { data = results
                , columns =
                    [ { header = songListHeader "Song Title"
                      , width = fillPortion 3
                      , view =
                            \songData ->
                                songListCell
                                    options
                                    [ Font.alignLeft
                                    , inFront <| addRemoveButton songData
                                    ]
                                    songData.title
                      }
                    , { header = songListHeader "Artist Name"
                      , width = fillPortion 3
                      , view =
                            .artist
                                >> songListCell options
                                    [ Border.widthXY 1 0
                                    , Border.solid
                                    , Font.alignLeft
                                    ]
                      }
                    , { header = songListHeader "Code"
                      , width = fillPortion 1
                      , view =
                            \songData ->
                                songListCell options
                                    []
                                    songData.code
                      }
                    ]
                }
            ]


addRemoveButton : SongData -> Element Msg
addRemoveButton songData =
    let
        ( onPress, labelText, titleText ) =
            case songData.key of
                Just key ->
                    ( Just (RemoveSong key), "⊖", "Remove from wishlist" )

                Nothing ->
                    ( Just (AddSong songData), "⊕", "Add to wishlist" )
    in
    Input.button
        [ Font.color white
        , paddingEach
            { top = 0
            , right = 20
            , bottom = 10
            , left = 2
            }
        , Font.size 14
        , htmlAttribute <| title titleText
        ]
        { onPress = onPress, label = text labelText }


songListHeader : String -> Element Msg
songListHeader headerText =
    text headerText
        |> el
            [ Font.bold
            , Font.center
            , Font.size 12
            , padding 10
            , Border.widthEach
                { top = 0
                , right = 0
                , bottom = 1
                , left = 0
                }
            ]


songListCell : SongListOptions -> List (Attribute Msg) -> String -> Element Msg
songListCell { charLimit } attributes cellText =
    ellipsis charLimit cellText
        |> text
        |> el ([ padding 10, Font.center, Font.size 12 ] ++ attributes)


authWrapper : Model -> Element Msg
authWrapper model =
    column [ Font.color pink, Font.size 12, Events.onMouseLeave CloseAuthForm ]
        (case model.uid of
            Just uid ->
                [ Input.button [ padding 10 ]
                    { onPress = Just SignOut
                    , label = text "Sign Out"
                    }
                ]

            Nothing ->
                [ row []
                    [ Input.button []
                        { onPress = Just (ShowAuthForm SignIn)
                        , label =
                            el
                                [ Background.color
                                    (if model.authForm == Just SignIn then
                                        transparentPurple

                                     else
                                        black
                                    )
                                , padding 10
                                ]
                                (text "Sign In ⌄")
                        }
                    , Input.button []
                        { onPress = Just (ShowAuthForm SignUp)
                        , label =
                            el
                                [ Background.color
                                    (if model.authForm == Just SignUp then
                                        transparentPurple

                                     else
                                        black
                                    )
                                , padding 10
                                ]
                                (text "Sign Up ⌄")
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
        [ Input.email [ Background.color purple ]
            { onChange = ChangeEmail
            , text = model.email
            , placeholder = Nothing
            , label = Input.labelAbove [] <| text "email"
            }
        , passwordField [ Background.color transparentPurple ]
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


port receiveError : (String -> msg) -> Sub msg



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

port module Main exposing (main)

import Browser
import Browser.Events
import Debounce
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes exposing (style, title)
import Http
import Json.Decode as Decode exposing (Decoder, field, maybe, string)
import String.Extra exposing (ellipsis)
import Toasty
import Toasty.Defaults
import Url.Builder


main : Program WindowSize Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { authForm : Maybe AuthForm
    , debounce : Debounce.Debounce String
    , device : Device
    , email : String
    , language : Language
    , password : String
    , query : String
    , results : List SongData
    , showResults : Bool
    , showWishListModal : Bool
    , toasties : Toasty.Stack Toasty.Defaults.Toast
    , uid : Maybe String
    , wishList : List SongData
    }


type alias WindowSize =
    { width : Int
    , height : Int
    }


type Language
    = English
    | Spanish


type AuthForm
    = SignInForm
    | SignUpForm


type alias SongData =
    { key : Maybe String
    , code : String
    , title : String
    , artist : String
    }


initialModel : Device -> Model
initialModel device =
    { authForm = Nothing
    , debounce = Debounce.init
    , device = device
    , email = ""
    , language = English
    , password = ""
    , query = ""
    , results = []
    , showResults = False
    , showWishListModal = False
    , toasties = Toasty.initialState
    , uid = Nothing
    , wishList = []
    }


init : WindowSize -> ( Model, Cmd Msg )
init windowSize =
    ( initialModel <| classifyDevice windowSize, Cmd.none )


languageToString : Language -> String
languageToString language =
    case language of
        English ->
            "English"

        Spanish ->
            "Spanish"


toastyConfig : Toasty.Config Msg
toastyConfig =
    Toasty.Defaults.config
        |> Toasty.containerAttrs
            [ style "max-width" "300px"
            , style "position" "fixed"
            , style "right" "0"
            , style "top" "0"
            , style "font-family" "Roboto Mono"
            , style "list-style" "none"
            ]


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later 500
    , transform = DebounceMsg
    }



-- UPDATE


type Msg
    = AddSong SongData
    | ApiResponse (Result Http.Error (List SongData))
    | ChangeEmail String
    | ChangePassword String
    | CloseAuthForm
    | CloseWishListModal
    | CreateUser
    | DebounceMsg Debounce.Msg
    | OpenWishListModal
    | QueryChange String
    | ReceiveError String
    | ReceiveUid (Maybe String)
    | ReceiveWishList (Result Decode.Error (List SongData))
    | RemoveSong String
    | SelectLanguage Language
    | SignInUser
    | SignOut
    | SongAdded (Result Decode.Error SongData)
    | SongRemoved String
    | ShowAuthForm AuthForm
    | ToastyMsg (Toasty.Msg Toasty.Defaults.Toast)
    | WindowResize Int Int


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
                            (Toasty.Defaults.Warning
                                "Dammit, Janet!"
                                "Please sign in to add songs to your wish list."
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

        CloseWishListModal ->
            ( { model | showWishListModal = False }, Cmd.none )

        CreateUser ->
            ( model, createUser ( model.email, model.password ) )

        DebounceMsg subMsg ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        debounceConfig
                        (Debounce.takeLast <| apiGet model.language)
                        subMsg
                        model.debounce
            in
            ( { model | debounce = debounce }
            , cmd
            )

        OpenWishListModal ->
            ( { model | showWishListModal = True }, Cmd.none )

        QueryChange newQuery ->
            let
                ( debounce, cmd ) =
                    Debounce.push debounceConfig newQuery model.debounce
            in
            ( { model | query = newQuery, debounce = debounce }, cmd )

        ReceiveError message ->
            ( model, Cmd.none )
                |> Toasty.addToast
                    toastyConfig
                    ToastyMsg
                    (Toasty.Defaults.Error "Bismillah NO!" message)

        ReceiveUid newUid ->
            case newUid of
                Just _ ->
                    ( { model | uid = newUid, email = "", password = "" }, Cmd.none )
                        |> Toasty.addToast toastyConfig
                            ToastyMsg
                            (Toasty.Defaults.Success
                                "Heathcliff, it's me, I'm Cathy!"
                                "You've successfully signed in."
                            )

                Nothing ->
                    ( { model | uid = newUid }, Cmd.none )

        ReceiveWishList result ->
            case result of
                Ok wishList ->
                    ( { model | wishList = wishList }, Cmd.none )

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
                        wishList =
                            model.wishList ++ [ songData ]
                    in
                    ( { model | wishList = wishList }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        SongRemoved key ->
            let
                wishList =
                    model.wishList
                        |> List.filter (\song -> song.key /= Just key)

                showWishListModal =
                    if List.isEmpty wishList then
                        False

                    else
                        model.showWishListModal
            in
            ( { model
                | wishList = wishList
                , showWishListModal = showWishListModal
              }
            , Cmd.none
            )

        ShowAuthForm formType ->
            ( { model | authForm = Just formType }, Cmd.none )

        ToastyMsg subMsg ->
            Toasty.update toastyConfig ToastyMsg subMsg model

        WindowResize width height ->
            let
                windowSize =
                    WindowSize width height
            in
            ( { model | device = classifyDevice windowSize }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize WindowResize
        , receiveNewUid ReceiveUid
        , receiveError ReceiveError
        , (Decode.decodeValue songDataListDecoder >> ReceiveWishList)
            |> receiveWishList
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
            , inFront <| wishListModalButton model
            , inFront <| wishListModal model
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
                , searchBox model
                , row [ width fill, spacing 10 ]
                    [ if model.showResults then
                        column [ alignTop, width <| fillPortion 3 ]
                            [ el [ centerX, Font.size 12, padding 10 ] <| text "Search Results"
                            , songList
                                { charLimit = 30
                                , background = black
                                , font = pink
                                , device = model.device
                                }
                                model.results
                            ]

                      else
                        none
                    , if showWishListByResults model then
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
                            , songList
                                { charLimit = charLimit
                                , background =
                                    purple
                                , font = white
                                , device = model.device
                                }
                                model.wishList
                            ]

                      else
                        none
                    ]
                ]
        , Toasty.view toastyConfig Toasty.Defaults.view ToastyMsg model.toasties
        ]
    }


showWishListByResults : Model -> Bool
showWishListByResults model =
    case model.device.class of
        Phone ->
            False

        Tablet ->
            False

        _ ->
            not <| List.isEmpty model.wishList


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


searchBox : Model -> Element Msg
searchBox model =
    let
        ( fontSize, maxWidth ) =
            case model.device.class of
                Phone ->
                    ( 14, 350 )

                _ ->
                    ( 16, 600 )
    in
    el [ width <| maximum maxWidth <| fill, centerX ]
        (Input.search
            [ width fill
            , Background.color black
            , Border.color pink
            , Border.rounded 5
            , Font.size fontSize
            ]
            { onChange = QueryChange
            , text = model.query
            , placeholder = searchInputPlaceholder
            , label = Input.labelHidden "Search"
            }
        )


searchInputPlaceholder : Maybe (Input.Placeholder Msg)
searchInputPlaceholder =
    text "Enter a song or artist name"
        |> Input.placeholder [ Font.color gray, clip ]
        |> Just


type alias SongListOptions =
    { charLimit : Int
    , background : Color
    , font : Color
    , device : Device
    }


songList : SongListOptions -> List SongData -> Element Msg
songList options results =
    let
        ( fontSize, maxWidth, charLimit ) =
            case options.device.class of
                Phone ->
                    ( 10, 350, options.charLimit // 3 * 2 )

                _ ->
                    ( 12, 600, options.charLimit )

        filteredOptions =
            { options | charLimit = charLimit }
    in
    if List.isEmpty results then
        el [ centerX ] <| text "No results found"

    else
        column [ width <| maximum maxWidth <| fill, centerX ]
            [ Element.table
                [ width fill
                , Border.width 1
                , Border.rounded 5
                , clipY
                , scrollbarY
                , height <| maximum 350 <| fill
                , Background.color options.background
                , Font.color options.font
                , Font.size fontSize
                ]
                { data = results
                , columns =
                    [ { header = songListHeader "Song Title"
                      , width = fillPortion 3
                      , view =
                            \songData ->
                                songListCell
                                    filteredOptions
                                    [ Font.alignLeft
                                    , inFront <| addRemoveButton songData
                                    ]
                                    songData.title
                      }
                    , { header = songListHeader "Artist Name"
                      , width = fillPortion 3
                      , view =
                            .artist
                                >> songListCell filteredOptions
                                    [ Border.widthXY 1 0
                                    , Border.solid
                                    , Font.alignLeft
                                    ]
                      }
                    , { header = songListHeader "Code"
                      , width = fillPortion 1
                      , view =
                            \songData ->
                                songListCell filteredOptions
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
                    ( Just (RemoveSong key), "⊖", "Remove from wish list" )

                Nothing ->
                    ( Just (AddSong songData), "⊕", "Add to wish list" )
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
        |> el ([ padding 10, Font.center ] ++ attributes)


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
                let
                    { signInMsg, signUpMsg, signInBg, signUpBg } =
                        case model.authForm of
                            Just SignInForm ->
                                { signInMsg = Just CloseAuthForm
                                , signUpMsg = Just (ShowAuthForm SignUpForm)
                                , signInBg = transparentPurple
                                , signUpBg = black
                                }

                            Just SignUpForm ->
                                { signInMsg = Just (ShowAuthForm SignInForm)
                                , signUpMsg = Just CloseAuthForm
                                , signInBg = black
                                , signUpBg = transparentPurple
                                }

                            Nothing ->
                                { signInMsg = Just (ShowAuthForm SignInForm)
                                , signUpMsg = Just (ShowAuthForm SignUpForm)
                                , signInBg = black
                                , signUpBg = black
                                }
                in
                [ row []
                    [ Input.button []
                        { onPress = signInMsg
                        , label =
                            el
                                [ Background.color signInBg
                                , padding 10
                                ]
                                (text "Sign In ⌄")
                        }
                    , Input.button []
                        { onPress = signUpMsg
                        , label =
                            el
                                [ Background.color signUpBg
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
                SignInForm ->
                    ( Input.currentPassword, Just SignInUser, "Sign In" )

                SignUpForm ->
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


wishListModalButton : Model -> Element Msg
wishListModalButton model =
    case model.device.class of
        Desktop ->
            none

        BigDesktop ->
            none

        _ ->
            if model.uid == Nothing || List.isEmpty model.wishList then
                none

            else
                Input.button [ Font.color pink, Font.size 12, padding 10, alignRight ]
                    { onPress = Just OpenWishListModal
                    , label =
                        row [ spacing 10 ]
                            [ text "Wish List"
                            , wishListCountBadge model.wishList
                            ]
                    }


wishListCountBadge : List SongData -> Element Msg
wishListCountBadge wishList =
    el
        [ Background.color purple
        , Font.center
        , Border.rounded 10
        , Font.color white
        , Border.width 1
        , padding 2
        ]
        (text <| String.fromInt (List.length wishList))


wishListModal : Model -> Element Msg
wishListModal model =
    if model.showWishListModal then
        column
            [ width fill
            , height fill
            , Background.color transparentPurple
            ]
            [ Input.button [ centerY, alignRight ]
                { onPress = Just CloseWishListModal
                , label =
                    el
                        [ rotate <| degrees 45
                        , Font.color white
                        , Font.size 26
                        , padding 30
                        ]
                    <|
                        text "+"
                }
            , el [ centerX, centerY ]
                (songList
                    { charLimit = 30
                    , background = purple
                    , font = white
                    , device = model.device
                    }
                    model.wishList
                )
            ]

    else
        none



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


port receiveWishList : (Decode.Value -> msg) -> Sub msg


port songRemoved : (String -> msg) -> Sub msg


port songAdded : (Decode.Value -> msg) -> Sub msg


port receiveError : (String -> msg) -> Sub msg



-- API


apiGet : Language -> String -> Cmd Msg
apiGet language query =
    let
        url =
            Url.Builder.crossOrigin "https://gagopa.herokuapp.com"
                [ "songs" ]
                [ Url.Builder.string "query" query
                , Url.Builder.string "language" (languageToString language)
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

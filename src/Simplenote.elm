module Simplenote exposing (main)

import Html exposing (Html, button, div, h1, h2, input, label, programWithFlags, text, textarea)
import Html.Attributes exposing (for, name, type_, value)
import Html.Events exposing (onClick, onInput)
import Process as Process
import Task as Task
import Time exposing (Time)
import WebSocket as WS
import Data.Simperium as Simperium
import Data.Stream as Stream
import Msg exposing (Msg(..))


type alias Flags =
    {}


type alias LoggedInModel =
    { connection : Stream.ConnectionInfo
    , manualCommand : String
    }


type alias LoggedOutModel =
    { username : String
    , password : String
    }


type Model
    = LoggedOut LoggedOutModel
    | LoggedIn LoggedInModel


delay : Time -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity


main : Program Flags Model Msg
main =
    programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        connection =
            Simperium.makeConnection
                { accessToken = "6eac88ed585e4d6ebfd1f9569f6b1889"
                , clientId = "client"
                }

        cmd =
            Simperium.connect connection
                |> Maybe.withDefault Cmd.none
    in
        ( LoggedOut { username = "", password = "" }, cmd )


subscriptions : Model -> Sub Msg
subscriptions appModel =
    case appModel of
        LoggedOut model ->
            Sub.none

        LoggedIn model ->
            Simperium.subscriptions FromSimperium model.connection.appId


update : Msg -> Model -> ( Model, Cmd Msg )
update msg appModel =
    case appModel of
        LoggedOut model ->
            case msg of
                Login accessToken _ ->
                    let
                        clientId =
                            "simplenote-elm"

                        connection =
                            Simperium.makeConnection { accessToken = accessToken, clientId = clientId }
                    in
                        ( LoggedIn { connection = connection, manualCommand = "" }
                        , Simperium.connect connection |> Maybe.withDefault Cmd.none
                        )

                SubmitLogin username password ->
                    ( appModel
                    , Simperium.authenticate { username = username, password = password }
                    )

                UpdatePassword s ->
                    ( LoggedOut { model | password = s }, Cmd.none )

                UpdateUsername s ->
                    ( LoggedOut { model | username = s }, Cmd.none )

                _ ->
                    ( appModel, Cmd.none )

        LoggedIn model ->
            case msg of
                FromSimperium s ->
                    Simperium.update ToSimperium s model.connection
                        |> Tuple.mapFirst (\c -> LoggedIn { model | connection = c })

                ToSimperium d s ->
                    ( appModel, Simperium.dispatch d s model.connection )

                Logout ->
                    ( LoggedOut { username = "", password = "" }, Cmd.none )

                SubmitManualCommand s ->
                    ( appModel
                    , WS.send "wss://api.simperium.com/sock/1/history-analyst-dad/websocket" s
                    )

                UpdateManualCommand s ->
                    ( LoggedIn { model | manualCommand = s }, Cmd.none )

                _ ->
                    ( appModel, Cmd.none )


view : Model -> Html Msg
view appModel =
    case appModel of
        LoggedOut model ->
            loggedOutView model

        LoggedIn model ->
            loggedInView model


loggedOutView : LoggedOutModel -> Html Msg
loggedOutView model =
    div []
        [ h1 [] [ text "Login" ]
        , div []
            [ label [ for "username" ] [ text "Username" ]
            , input
                [ name "username"
                , onInput UpdateUsername
                , value model.username
                ]
                []
            ]
        , div []
            [ label [ for "password" ] [ text "Password" ]
            , input
                [ name "password"
                , onInput UpdatePassword
                , type_ "password"
                , value model.password
                ]
                []
            ]
        , div []
            [ button
                [ onClick (SubmitLogin model.username model.password)
                ]
                [ text "Login" ]
            ]
        ]


loggedInView : LoggedInModel -> Html Msg
loggedInView model =
    div []
        [ h1 [] [ text "Simplenote" ]
        , div []
            [ label [ for "manualCommand" ] [ text "Command" ]
            , input [ onInput UpdateManualCommand, name "manualCommand", type_ "text" ] []
            , button [ onClick (SubmitManualCommand model.manualCommand) ] [ text "Send" ]
            ]
        , button [ onClick Logout ] [ text "Logout" ]
        ]

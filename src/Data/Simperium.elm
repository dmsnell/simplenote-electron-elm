module Data.Simperium
    exposing
        ( authenticate
        , connect
        , dispatch
        , makeConnection
        , subscriptions
        , update
        )

import Http
import HttpBuilder as HTTP
import Json.Decode as JD
import Json.Encode as JE
import WebSocket as WS
import Data.Stream exposing (..)
import Data.StreamParser exposing (fromStream)
import Process as Process
import Task as Task
import Time as Time
import Msg exposing (Msg(..))


type alias AuthResponse =
    { accessToken : String, email : String }



{-

   There are two Simplenote accounts:
       - One is for testing and may be randomly deleted
       - One is production and should be used carefully

   | Account    | App Id              | Auth Key                         |
   | ---------- | ------------------- | -------------------------------- |
   | Testing    | history-analyst-dad | be606bcfa3db4377bf488900281aa1cc |
   | Production | chalk-bump-f49      | b7aa724f34004583bcea0584c92f112c |

-}


appId : String
appId =
    "history-analyst-dad"


apiKey : String
apiKey =
    "be606bcfa3db4377bf488900281aa1cc"


type Bucket
    = NoteBucket
    | TagBucket


connect : ConnectionInfo -> Maybe (Cmd msg)
connect connection =
    ConnectToBucket connection "note" Nothing
        |> send connection.appId (toBucket NoteBucket)
        |> Maybe.map Tuple.second


authenticate : { username : String, password : String } -> Cmd Msg
authenticate { username, password } =
    HTTP.post ("https://auth.simperium.com/1/" ++ appId ++ "/authorize/")
        |> HTTP.withHeader "X-Simperium-API-Key" apiKey
        |> HTTP.withJsonBody
            (JE.object
                [ ( "username", JE.string username )
                , ( "password", JE.string password )
                ]
            )
        |> HTTP.withExpect
            (Http.expectJson <|
                JD.map2
                    AuthResponse
                    (JD.field "access_token" JD.string)
                    (JD.field "username" JD.string)
            )
        |> HTTP.send fromAuthResponse


fromAuthResponse : Result Http.Error { email : String, accessToken : String } -> Msg
fromAuthResponse r =
    case r of
        Err _ ->
            LoginFailed

        Ok { accessToken, email } ->
            Login accessToken email


makeConnection : { accessToken : AccessToken, clientId : ClientId } -> ConnectionInfo
makeConnection { accessToken, clientId } =
    { appId = appId
    , accessToken = accessToken
    , apiVersion = 1.1
    , clientId = clientId
    , libraryName = "simplenote-elm"
    , libraryVersion = 1
    }


send : AppId -> Destination -> StreamMsg -> Maybe ( String, Cmd msg )
send appId destination msg =
    let
        address =
            simperiumAddress appId

        channel =
            case destination of
                ToServer ->
                    ""

                ToChannel id ->
                    toString id ++ ":"
    in
        toStream (Debug.log ">Stream" msg)
            |> Maybe.map (\s -> ( channel ++ s, WS.send address (channel ++ s) ))


simperiumAddress : AppId -> String
simperiumAddress appId =
    "wss://api.simperium.com/sock/1/" ++ appId ++ "/websocket"


subscriptions : (String -> msg) -> AppId -> Sub msg
subscriptions tag appId =
    WS.listen (simperiumAddress appId) tag


toBucket : Bucket -> Destination
toBucket bucket =
    ToChannel <|
        case bucket of
            NoteBucket ->
                0

            TagBucket ->
                1


toStream : StreamMsg -> Maybe String
toStream msg =
    case msg of
        ConnectToBucket info bucketName msg ->
            let
                jsonInfo =
                    JE.object
                        [ ( "api", JE.float info.apiVersion )
                        , ( "app_id", JE.string info.appId )
                        , ( "client_id", JE.string info.clientId )
                        , ( "library", JE.string info.libraryName )
                        , ( "name", JE.string bucketName )
                        , ( "token", JE.string info.accessToken )
                        , ( "version", JE.float info.libraryVersion )
                        ]

                initialCommand =
                    case msg of
                        Just cmd ->
                            toStream cmd
                                |> Maybe.map (\s -> ":" ++ s)
                                |> Maybe.withDefault ""

                        Nothing ->
                            ""
            in
                "init:"
                    ++ JE.encode 0 jsonInfo
                    ++ initialCommand
                    |> Just

        Heartbeat n ->
            Just ("h:" ++ toString n)

        _ ->
            Nothing


delay : Time.Time -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity


dispatch : Destination -> StreamMsg -> ConnectionInfo -> Cmd msg
dispatch destination msg info =
    send info.appId destination msg
        |> Maybe.map Tuple.second
        |> Maybe.withDefault Cmd.none


update : (Destination -> StreamMsg -> msg) -> String -> ConnectionInfo -> ( ConnectionInfo, Cmd msg )
update queue msg info =
    let
        next =
            fromStream msg
                |> Maybe.map (Debug.log "<Stream")

        cmd =
            case next of
                Just (AuthValid _) ->
                    send info.appId ToServer (Heartbeat 1)
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault Cmd.none

                Just (Heartbeat i) ->
                    delay (Time.second * 3) (queue ToServer (Heartbeat <| i + 1))

                _ ->
                    Cmd.none
    in
        ( info, cmd )

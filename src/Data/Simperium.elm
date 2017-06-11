module Data.Simperium
    exposing
        ( connect
        , dispatch
        , makeConnection
        , subscriptions
        , update
        )

import Json.Encode as J
import WebSocket as WS
import Data.Stream exposing (..)
import Data.StreamParser exposing (fromStream)
import Process as Process
import Task as Task
import Time as Time


type Bucket
    = NoteBucket
    | TagBucket


connect : ConnectionInfo -> Maybe (Cmd msg)
connect connection =
    ConnectToBucket connection "note" Nothing
        |> send connection.appId (toBucket NoteBucket)
        |> Maybe.map Tuple.second


makeConnection : { accessToken : AccessToken, clientId : ClientId } -> ConnectionInfo
makeConnection { accessToken, clientId } =
    { appId = "chalk-bump-f49"
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
        toStream msg
            |> Maybe.map (\s -> ( Debug.log ">Stream" (channel ++ s), WS.send address (channel ++ s) ))


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
                    J.object
                        [ ( "api", J.float info.apiVersion )
                        , ( "app_id", J.string info.appId )
                        , ( "client_id", J.string info.clientId )
                        , ( "library", J.string info.libraryName )
                        , ( "name", J.string bucketName )
                        , ( "token", J.string info.accessToken )
                        , ( "version", J.float info.libraryVersion )
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
                    ++ J.encode 0 jsonInfo
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

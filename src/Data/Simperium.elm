module Data.Simperium
    exposing
        ( connect
        , makeConnection
        , subscriptions
        , update
        )

import Json.Encode as J
import WebSocket as WS
import Data.Stream exposing (..)
import Data.StreamParser exposing (fromStream)


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


update : String -> ConnectionInfo -> ( ConnectionInfo, Cmd msg )
update msg info =
    let
        next =
            fromStream msg
                |> Maybe.map (Debug.log "Stream")
    in
        ( info, Cmd.none )

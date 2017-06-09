module Data.Simperium
    exposing
        ( AccessToken
        , ClientId
        , ConnectionInfo
        , connect
        , makeConnection
        , subscriptions
        , update
        )

import Json.Decode as JD
import Json.Encode as J
import Data.Note exposing (Email(..))
import Parser exposing (Count(Exactly), Parser, andThen, fail, int, keep, keyword, map, oneOf, oneOrMore, run, succeed, symbol, zeroOrMore, (|.), (|=))
import WebSocket as WS


type alias AccessToken =
    String


type alias ApiVersion =
    Float


type alias AppId =
    String


type AuthError
    = TokenFormatInvalid
    | TokenInvalid
    | UnknownAuthError String


type alias BucketName =
    String


type Bucket
    = NoteBucket
    | TagBucket


type alias ChannelId =
    Int


type alias ClientId =
    String


connect : ConnectionInfo -> Maybe (Cmd msg)
connect connection =
    ConnectToBucket connection "note" Nothing
        |> send connection.appId (toBucket NoteBucket)
        |> Maybe.map Tuple.second


type alias ConnectionInfo =
    { appId : AppId
    , accessToken : AccessToken
    , apiVersion : ApiVersion
    , clientId : ClientId
    , libraryName : LibraryName
    , libraryVersion : LibraryVersion
    }


type Destination
    = ToChannel ChannelId
    | ToServer


type ParsedStream
    = ParsedStream (Maybe Destination) StreamMsg


toEnd : Parser String
toEnd =
    keep zeroOrMore (\_ -> True)


jsonToEnd : Parser String
jsonToEnd =
    succeed (++)
        |= keep (Exactly 1) (\c -> c == '{')
        |= toEnd


fromAuthErrorCode : Int -> AuthError
fromAuthErrorCode code =
    case code of
        400 ->
            TokenFormatInvalid

        401 ->
            TokenInvalid

        _ ->
            UnknownAuthError ("Unknown failure code " ++ toString code)


fromInvalidAuth : String -> StreamMsg
fromInvalidAuth s =
    JD.map fromAuthErrorCode (JD.field "code" JD.int)
        |> (flip JD.decodeString) s
        |> Result.withDefault (UnknownAuthError s)
        |> AuthInvalid


streamParser : Parser ParsedStream
streamParser =
    succeed ParsedStream
        |= map (Just << ToChannel) int
        |. (symbol ":auth:")
        |= (oneOf
                [ map fromInvalidAuth jsonToEnd
                , map (AuthValid << Email) toEnd
                ]
           )


fromStream : String -> Maybe StreamMsg
fromStream s =
    let
        _ =
            run streamParser s
                |> Debug.log "Msg Parse"
    in
        Nothing


makeConnection : { accessToken : AccessToken, clientId : ClientId } -> ConnectionInfo
makeConnection { accessToken, clientId } =
    { appId = "chalk-bump-f49"
    , accessToken = accessToken
    , apiVersion = 1.1
    , clientId = clientId
    , libraryName = "simplenote-elm"
    , libraryVersion = 1
    }


type alias LibraryName =
    String


type alias LibraryVersion =
    Float


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


type StreamMsg
    = AuthInvalid AuthError
    | AuthValid Email
    | ConnectToBucket ConnectionInfo BucketName (Maybe StreamMsg)
    | Heartbeat Int


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
    in
        ( info, Cmd.none )

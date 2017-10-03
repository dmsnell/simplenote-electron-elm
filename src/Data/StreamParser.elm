module Data.StreamParser
    exposing
        ( fromStream
        )

import Parser exposing (..)
import Json.Decode as JD
import Data.Note exposing (Email(Email))
import Data.Stream exposing (..)


type ParsedStream
    = ChannelMessage Destination StreamMsg
    | ServerMessage StreamMsg


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


authMessage : Parser StreamMsg
authMessage =
    succeed identity
        |. (symbol "auth:")
        |= (oneOf
                [ map fromInvalidAuth jsonToEnd
                , map (AuthValid << Email) toEnd
                ]
           )


heartbeat : Parser ParsedStream
heartbeat =
    succeed ServerMessage
        |. (symbol "h:")
        |= map Heartbeat int


fromBucketIndex : String -> Parser StreamMsg
fromBucketIndex s =
    let
        decoder =
            JD.map3
                BucketIndexValue
                (JD.field "current" JD.string)
                (JD.field "index" JD.value)
                (JD.field "mark" JD.string)
    in
        JD.decodeString decoder s
            |> Result.map (succeed << BucketIndex)
            |> Result.withDefault (fail "invalid parse")


bucketIndex : Parser StreamMsg
bucketIndex =
    succeed identity
        |. (symbol "i:")
        |= andThen fromBucketIndex jsonToEnd


channelMessage : Parser ParsedStream
channelMessage =
    succeed ChannelMessage
        |= map ToChannel int
        |. (symbol ":")
        |= oneOf
            [ authMessage
            , bucketIndex
            ]


streamParser : Parser ParsedStream
streamParser =
    oneOf
        [ channelMessage
        , heartbeat
        ]


fromStream : String -> Maybe StreamMsg
fromStream s =
    case run streamParser s of
        Ok (ChannelMessage channel msg) ->
            Just msg

        Ok (ServerMessage msg) ->
            Just msg

        Err error ->
            let
                _ =
                    Debug.log "Stream Parse" error
            in
                Nothing

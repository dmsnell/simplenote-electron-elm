module Data.StreamParser
    exposing
        ( fromStream
        )

import Parser exposing (..)
import Json.Decode as JD
import Data.Note exposing (Email(Email))
import Data.Stream exposing (..)


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
    case run streamParser s of
        Ok (ParsedStream channel msg) ->
            case channel of
                Just destination ->
                    case msg of
                        _ ->
                            Just msg

                Nothing ->
                    case msg of
                        AuthValid email ->
                            Just msg

                        _ ->
                            Nothing

        Err error ->
            let
                _ =
                    Debug.log "Stream Parse" error
            in
                Nothing

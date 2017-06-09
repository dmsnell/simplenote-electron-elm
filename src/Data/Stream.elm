module Data.Stream exposing (..)

import Data.Note exposing (Email)


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


type alias ChannelId =
    Int


type alias ClientId =
    String


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


type alias LibraryName =
    String


type alias LibraryVersion =
    Float


type StreamMsg
    = AuthInvalid AuthError
    | AuthValid Email
    | ConnectToBucket ConnectionInfo BucketName (Maybe StreamMsg)
    | Heartbeat Int

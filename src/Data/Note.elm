module Data.Note
    exposing
        ( Email
        , Note
        , NoteReference
        , Tag
        )

import Date exposing (Date)
import Set exposing (Set)


type ContentType
    = Plaintext
    | Markdown


type Email
    = Email String


type alias Note =
    { content : String
    , contentType : ContentType
    , createdAt : Date
    , flags : NoteFlags
    , id : UUID
    , options : Set NoteOption
    , revisionId : RevisionId
    , revisions : List Revision
    , tags : Set Tag
    , updatedAt : Date
    }


type alias NoteFlags =
    { isDeleted : Bool
    , isPinned : Bool
    }


type NoteOption
    = Published (Maybe PublishKey)
    | Shared (Set Email)


type NoteReference
    = LatestKnown UUID
    | Snapshot UUID Revision


type PublishKey
    = PublishKey String


type alias Revision =
    { id : RevisionId
    , updatedAt : Date
    }


type RevisionId
    = RevisionId Int


type Tag
    = Tag String


type UUID
    = UUID String

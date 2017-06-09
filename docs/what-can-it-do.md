# What can Simplenote do?

## App operations

### Authentication

> login : Email -> Password -> Result LoginFailure AuthToken
>
> type LoginFailure = InvalidCredentials | UnspecifiedFailure
>
> logout : Unit

## Note operations

### Editing

Editing notes is the primary purpose of Simplenote. Changes to
individual notes are inherently _differential_ though we have
the opportunity to ignore history and wipe out a note with
entirely new content.

>
> type Edit
>   = Delete Range
>   | Insert Offset String
>   | Replace Range String
>   | Reset String
>

Ranges start at an offset and go until the end of an offset.

>
> type alias Range = ( Offset, Offset )
>

Edit operations can intrinsically fail because this is a
distributed application. Failures _only_ occur when there
are merge conflicts (almost true). Therefore, if we cannot
succesfully apply an update we will return with the most
recent revision provided by the server.

>
> edit : Note -> Edit -> Result Note Note
>

Some operations however cannot fail. They may have issues
when synchronizing with Simperium, but that is a different
layer for the app that Simperium will handle.

>
> pin : Note -> Note
> unpin : Note -> Note
>
> trash : Note -> Note
> untrash : Note -> Note
>
> setContentType : ContentType -> Note -> Note 
>
> addTag : Tag -> Note -> Note
> removeTag : Tag -> Note -> Note
>

some operations can occur succesfully without fully finishing
because they rely on action from the Simplenote servers. these
operations change the note state but the other half of the
action must occur on the server when it detects the appropriate
changes to the note object in Simperium. that is, we can have a
published note without a publish key because that key comes from
a remote note update in response to our action.

>
> addShare : Note -> Email -> Note
> removeShare : Note -> Email -> Note
>
> publishNote : Note -> Result String Note
> unpublishNote : Note -> Result String Note
>

### Access

Not all notes need be stored in the Simplenote app itself.
In fact, if we did this as the default then we would risk hitting
some significant performance issues for accounts with lots of
content. We would end up duplicating data in memory: the Simperium
layer already needs to store its own data to handle the exchanges
between the server and our "local client."

The note list can be used to generate the "cache" or simplified
view of the notes. The return value is the result of mapping over
the note data with the provided function. Typically we would
consider returning a title, note excerpt, and note flags which are
needed to render the list of notes.

Of course, we can also pass the identity function to get the full
list of notes should we need it. For example, when generating an
export of the notes in the acocunt.

If the mapping function returns nothing then the note will not
appear in the returned note list. We can use this to perform a
search, for example, without loading full duplicates of the results
into the app.

>
> getNotes : (Note -> Maybe a) -> NoteList a
>

For grabbing a specific note we could send a predicate matching on
the note id but that would result in iteration through the entire
note list despite lookup being available. We can use the faster
version directly.

However, since we are asking for a specific note we must acnowledge
that it may no longer exist.

>
> getNote : NoteReference -> Maybe Note
>


module Simplenote exposing (main)

import Html exposing (Html, button, div, h1, h2, input, programWithFlags, text, textarea)
import Process as Process
import Task as Task
import Time exposing (Time)
import Data.Simperium as Simperium
import Data.Stream as Stream


type alias Flags =
    {}


type alias Model =
    { connection : Stream.ConnectionInfo
    }


type Msg
    = FromSimperium String
    | ToSimperium Stream.Destination Stream.StreamMsg


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
                { accessToken = "token"
                , clientId = "client"
                }

        cmd =
            Simperium.connect connection
                |> Maybe.withDefault Cmd.none
    in
        ( { connection = connection }, cmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Simperium.subscriptions FromSimperium model.connection.appId


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FromSimperium s ->
            Simperium.update ToSimperium s model.connection
                |> Tuple.mapFirst (\c -> { model | connection = c })

        ToSimperium d s ->
            ( model, Simperium.dispatch d s model.connection )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Simplenote Elm" ]
        ]

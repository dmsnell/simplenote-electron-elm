module Simplenote exposing (main)

import Html exposing (Html, button, div, h1, h2, input, programWithFlags, text, textarea)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onClick, onInput)
import Process as Process
import Task as Task
import Time exposing (Time)
import WebSocket exposing (listen, send)
import Data.Simperium as Stream


type alias Flags =
    {}


type alias Model =
    { connection : Stream.ConnectionInfo
    , fromSimperium : List String
    , heartbeat : Int
    , simperiumBuffer : String
    , toSimperium : List String
    }


type Msg
    = FromSimperium String
    | SendSimperiumBuffer
    | ToSimperium Stream.Destination Stream.StreamMsg
    | UpdateSimperiumBuffer String


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
        initModel =
            { connection = Stream.makeConnection { accessToken = "token", clientId = "client" }
            , fromSimperium = []
            , heartbeat = 0
            , simperiumBuffer = ""
            , toSimperium = []
            }

        connect =
            Stream.ConnectToBucket initModel.connection "note" Nothing |> ToSimperium (Stream.ToChannel 0)
    in
        update connect initModel


subscriptions : Model -> Sub Msg
subscriptions model =
    listen (Stream.simperiumAddress model.connection) FromSimperium


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FromSimperium s ->
            ( { model
                | fromSimperium = List.append model.fromSimperium [ s ]
                , heartbeat = model.heartbeat + 2
              }
            , delay (Time.second * 3) (ToSimperium Stream.ToServer (Stream.Heartbeat model.heartbeat))
            )

        SendSimperiumBuffer ->
            let
                buffer =
                    model.simperiumBuffer
            in
                ( { model
                    | simperiumBuffer = ""
                    , toSimperium = List.append model.toSimperium [ buffer ]
                  }
                , send (Stream.simperiumAddress model.connection) buffer
                )

        ToSimperium destination streamMsg ->
            case Stream.send model.connection destination streamMsg of
                Just ( buffer, cmd ) ->
                    ( { model | toSimperium = List.append model.toSimperium [ buffer ] }, cmd )

                Nothing ->
                    ( model, Cmd.none )

        UpdateSimperiumBuffer s ->
            ( { model | simperiumBuffer = s }, Cmd.none )


view : Model -> Html Msg
view { fromSimperium, simperiumBuffer, toSimperium } =
    let
        logStyle =
            style
                [ ( "width", "100%" )
                , ( "min-height", "200px" )
                ]
    in
        div []
            [ h1 [] [ text "Simplenote Elm" ]
            , h2 [] [ text "To Simperium:" ]
            , textarea [ logStyle ] [ text <| String.join "\n\n" toSimperium ]
            , h2 [] [ text "From Simperium:" ]
            , textarea [ logStyle ] [ text <| String.join "\n\n" fromSimperium ]
            , div []
                [ input
                    [ style [ ( "min-width", "50%" ) ]
                    , onInput UpdateSimperiumBuffer
                    , type_ "text"
                    , value simperiumBuffer
                    ]
                    []
                , button [ onClick SendSimperiumBuffer ] [ text "Send to Simperium" ]
                ]
            ]

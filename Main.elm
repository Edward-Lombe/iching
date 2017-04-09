module Main exposing (..)

import Html exposing (Html, text, program)
import Json.Decode as Decode exposing (Decoder)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (fill, width, height, viewBox, x, y)
import Http
import Window
import Task


-- MAIN


main : Program Never Model Message
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODELS


type Line
    = Broken
    | Unbroken


type alias Hexagram =
    ( Line, Line, Line, Line, Line, Line )


type alias IChing =
    List Hexagram


type alias Model =
    { iChing : IChing
    , size : Window.Size
    , bar_height : Int
    , bar_width : Int
    , hexagram_height : Int
    , hexagram_width : Int
    }


{-| The y-size of the bar
-}
bar_height : Int
bar_height =
    10


{-| The x-size of the bar
-}
bar_width : Int
bar_width =
    bar_height * 6 * 2


{-| The y-size of the hexagram
-}
hexagram_height : Int
hexagram_height =
    bar_height * 2 * 8


{-| The x-size of the hexagram
-}
hexagram_width : Int
hexagram_width =
    bar_height * 2 * 8


iChingURL : String
iChingURL =
    "iching.json"


init : ( Model, Cmd Message )
init =
    let
        getIChing : Cmd Message
        getIChing =
            iChingDecoder
                |> Http.get iChingURL
                |> Http.send ReceiveIChing

        getSize : Cmd Message
        getSize =
            Window.size
                |> Task.perform ReceiveSize

        effects : Cmd Message
        effects =
            [ getSize, getIChing ]
                |> Cmd.batch

        size : Window.Size
        size =
            { width = 0, height = 0 }

        initialModel : Model
        initialModel =
            { iChing = []
            , size = size
            , bar_height = bar_height
            , bar_width = bar_width
            , hexagram_height = hexagram_height
            , hexagram_width = hexagram_width
            }
    in
        ( initialModel, effects )



-- MESSAGES


type Message
    = NoOp
    | ReceiveIChing (Result Http.Error IChing)
    | ReceiveSize Window.Size



-- UPDATES


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    (case message of
        ReceiveIChing result ->
            receiveIChing result

        ReceiveSize size ->
            receiveSize size

        NoOp ->
            always <| model ! []
    )
        |> apply model


apply : a -> (a -> b) -> b
apply a fn =
    fn a


receiveIChing : Result Http.Error IChing -> Model -> ( Model, Cmd Message )
receiveIChing result model =
    case result of
        Ok iChing ->
            { model | iChing = iChing } ! []

        Err error ->
            let
                _ =
                    Debug.log "error fetching iChing" (error)
            in
                model ! []


receiveSize : Window.Size -> Model -> ( Model, Cmd Message )
receiveSize size model =
    { model | size = size } ! []


iChingDecoder : Decoder IChing
iChingDecoder =
    Decode.at [ "pattern" ] Decode.string
        |> Decode.map pattern2hexagram
        |> Decode.list
        |> Decode.at [ "hexagrams", "hexagram" ]


pattern2hexagram : String -> Hexagram
pattern2hexagram pattern =
    case String.split "" pattern of
        [ one, two, three, four, five, six ] ->
            ( one, two, three, four, five, six )
                |> map6tuple string2bar

        pattern ->
            let
                _ =
                    Debug.log "Invalid pattern" (pattern)
            in
                ( Broken, Broken, Broken, Broken, Broken, Broken )


map6tuple : (a -> b) -> ( a, a, a, a, a, a ) -> ( b, b, b, b, b, b )
map6tuple fn ( one, two, three, four, five, six ) =
    ( fn one, fn two, fn three, fn four, fn five, fn six )


listFrom6tuple : ( a, a, a, a, a, a ) -> List a
listFrom6tuple ( one, two, three, four, five, six ) =
    [ one, two, three, four, five, six ]


string2bar : String -> Line
string2bar string =
    case string of
        "9" ->
            Unbroken

        "6" ->
            Broken

        unknown ->
            let
                _ =
                    Debug.log "Expected either 6 ot 9, found" unknown
            in
                Broken



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Message
subscriptions model =
    Window.resizes ReceiveSize



-- VIEWS


view : Model -> Html Message
view model =
    Html.div []
        [ viewHexagrams model
        ]


viewHexagrams : Model -> Html Message
viewHexagrams model =
    let
        width_ : Svg.Attribute Message
        width_ =
            hexagram_width
                * 8
                |> toString
                |> width

        height_ : Svg.Attribute Message
        height_ =
            hexagram_height
                * 8
                |> toString
                |> height
    in
        model.iChing
            |> List.indexedMap viewHexagram
            |> svg [ width_, height_ ]


viewHexagram : Int -> Hexagram -> Svg Message
viewHexagram position hexagram =
    let
        x_ : Svg.Attribute Message
        x_ =
            position
                % 8
                |> (*) hexagram_width
                |> toString
                |> x

        y_ : Svg.Attribute Message
        y_ =
            position
                // 8
                |> (*) hexagram_height
                |> toString
                |> y

        width_ : Svg.Attribute Message
        width_ =
            hexagram_width
                |> toString
                |> width

        height_ : Svg.Attribute Message
        height_ =
            hexagram_height
                |> toString
                |> height
    in
        hexagram
            |> listFrom6tuple
            |> List.indexedMap viewLine
            |> svg [ width_, height_, x_, y_ ]


viewLine : Int -> Line -> Svg Message
viewLine position bar =
    let
        y_ : Svg.Attribute Message
        y_ =
            position
                * 2
                |> (*) bar_height
                |> toString
                |> y

        bar_height_ : Svg.Attribute Message
        bar_height_ =
            bar_height
                |> toString
                |> height

        bar_width_ : Svg.Attribute Message
        bar_width_ =
            bar_width
                |> toString
                |> width

        broken_bar_width : Svg.Attribute Message
        broken_bar_width =
            bar_width
                // 3
                |> toString
                |> width

        x_offset : Svg.Attribute Message
        x_offset =
            bar_width
                // 3
                |> (*) 2
                |> toString
                |> x

        fill_ : Svg.Attribute Message
        fill_ =
            fill "black"

        attributes : List (Svg.Attribute Message)
        attributes =
            [ bar_height_, bar_width_, y_ ]

        bars : List (Svg Message)
        bars =
            if bar == Unbroken then
                [ rect [ bar_width_, bar_height_, fill_ ] []
                ]
            else
                [ rect [ broken_bar_width, bar_height_, fill_ ] []
                , rect [ broken_bar_width, bar_height_, fill_, x_offset ] []
                ]
    in
        svg attributes bars

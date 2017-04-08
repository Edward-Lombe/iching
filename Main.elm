module Main exposing (..)

import Html exposing (Html, text, program)
import Json.Decode as Decode exposing (Decoder)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (fill, width, height, viewBox, x, y)
import Http


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
    }


iChingURL : String
iChingURL =
    "https://raw.githubusercontent.com/la11111/willie-modules/master/texts_iching/iching.json"


init : ( Model, Cmd Message )
init =
    let
        effects =
            Http.get iChingURL iChingDecoder
                |> Http.send ReceiveIChing
    in
        ( { iChing = [] }, effects )



-- MESSAGES


type Message
    = NoOp
    | ReceiveIChing (Result Http.Error IChing)



-- UPDATES


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        ReceiveIChing result ->
            case result of
                Ok iChing ->
                    { model | iChing = iChing } ! []

                Err error ->
                    let
                        _ =
                            Debug.log "error fetching iChing" (error)
                    in
                        model ! []

        _ ->
            model ! []


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
                    Debug.log "Invalid patter" (pattern)
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
    Sub.none



-- VIEWS


view : Model -> Html Message
view model =
    Html.div []
        [ viewHexagrams model
        ]


viewHexagrams : Model -> Html Message
viewHexagrams model =
    model.iChing
        |> List.indexedMap viewHexagram
        |> svg [ width "1280", height "1280" ]


viewHexagram : Int -> Hexagram -> Svg Message
viewHexagram position hexagram =
    let
        x_ =
            position
                % 8
                |> (*) 160
                |> toString
                |> x

        y_ =
            position
                // 8
                |> (*) 160
                |> toString
                |> y
    in
        hexagram
            |> listFrom6tuple
            |> List.indexedMap viewBar
            |> svg [ width "160", height "160", x_, y_ ]


viewBar : Int -> Line -> Svg Message
viewBar position bar =
    let
        y_ =
            position
                * 20
                |> toString
                |> y

        height_ =
            height "10"

        fill_ =
            fill "black"

        attributes =
            [ width "120", height "10", y_ ]

        bars =
            if bar == Unbroken then
                [ rect [ width "120", height_, fill_ ] []
                ]
            else
                [ rect [ width "40", height_, fill_ ] []
                , rect [ width "40", height_, fill_, x "80" ] []
                ]
    in
        svg attributes bars

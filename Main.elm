module Main exposing (..)

import Html exposing (Html, program)
import Json.Decode as Decode exposing (Decoder)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (fill, width, height, x, y)
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
    , line_height : Int
    , line_width : Int
    , hexagram_height : Int
    , hexagram_width : Int
    }


iChingURL : String
iChingURL =
    "iching.json"


init : ( Model, Cmd Message )
init =
    let
        line_height : Int
        line_height =
            10

        line_width : Int
        line_width =
            line_height * 2 * 6

        hexagram_height : Int
        hexagram_height =
            line_height * 2 * 8

        hexagram_width : Int
        hexagram_width =
            line_height * 2 * 8

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
            , line_height = line_height
            , line_width = line_width
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


{-| Takes a result containing the decoded IChing and puts it into the model,
logs the error if there is one
-}
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


{-| Takes the size and updates model sizes in order to resize rending to screen
-}
receiveSize : Window.Size -> Model -> ( Model, Cmd Message )
receiveSize size model =
    let
        _ =
            Debug.log "new sizes"
                ( newHexagramHeight
                , newHexagramWidth
                , newLineHeight
                , newLineWidth
                )

        base_size =
            if size.height > size.width then
                size.width
            else
                size.height

        ( newHexagramHeight, newHexagramWidth, newLineHeight, newLineWidth ) =
            ( base_size // 8
            , base_size // 8
            , (base_size // 128)
            , (base_size // 32) * 3
            )
    in
        { model
            | size =
                size
            , line_height = newLineHeight
            , line_width = newLineWidth
            , hexagram_height = newHexagramHeight
            , hexagram_width = newHexagramWidth
        }
            ! []


{-| Decodes some JSON that represents the I Ching.

    exampleJSON : String
    exampleJSON =
        """
    {
        "hexagrams": {
            "hexagram": [
                {
                    "pattern": "966696"
                }
            ]
        }
    }
    """

    -- Would be equal to [(Unbroken, Broken, Broken, Broken, Unbroken, Broken)]
    exampleIChing : IChing
    exampleIChing =
        exampleJSON
            |> Decode.decodeString iChingDecoder
            |> Result.withDefault []
-}
iChingDecoder : Decoder IChing
iChingDecoder =
    Decode.at [ "pattern" ] Decode.string
        |> Decode.map pattern2hexagram
        |> Decode.list
        |> Decode.at [ "hexagrams", "hexagram" ]


{-| Takes a string of length 6 containing "6" or "9" and returns a six tuple of
Lines. Invalid patterns will log an error and return a default hexagram.

    -- Equal to (Unbroken, Broken, Broken, Broken, Unbroken, Broken)
    exampleHexagram : Hexagram
    exampleHexagram =
        pattern2hexagram "966696"

-}
pattern2hexagram : String -> Hexagram
pattern2hexagram pattern =
    case String.split "" pattern of
        [ one, two, three, four, five, six ] ->
            ( one, two, three, four, five, six )
                |> map6tuple string2line

        pattern ->
            let
                _ =
                    Debug.log "Invalid pattern" (pattern)
            in
                ( Broken, Broken, Broken, Broken, Broken, Broken )


{-| Takes a function and applies it to each element of a 6 tuple

    -- Would be equal to ("1", "2", "3", "4", "5", "6")
    exampleTuple : (String, String, String, String, String, String)
    exampleTuple =
        (0, 1, 2, 3, 4, 5)
            |> map6tuple ((+) 1)
            |> map6tuple toString

-}
map6tuple : (a -> b) -> ( a, a, a, a, a, a ) -> ( b, b, b, b, b, b )
map6tuple fn ( one, two, three, four, five, six ) =
    ( fn one, fn two, fn three, fn four, fn five, fn six )


{-| Converts a 6 tuple to a list, useful for converting a tuple that will be
mapped over by a view function


    hexagram : Hexagram
    hexagram =
        ( Broken, Unbroken, Broken, Unbroken, Broken, Unbroken )

    viewHexagram : List (Svg Message)
    viewHexagram =
        hexagram
            |> listFrom6tuple
            |> List.indexedMap viewLine

-}
listFrom6tuple : ( a, a, a, a, a, a ) -> List a
listFrom6tuple ( one, two, three, four, five, six ) =
    [ one, two, three, four, five, six ]


string2line : String -> Line
string2line string =
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
viewHexagrams ({ hexagram_height, hexagram_width } as model) =
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
            |> List.indexedMap (viewHexagram model)
            |> svg [ width_, height_ ]


viewHexagram : Model -> Int -> Hexagram -> Svg Message
viewHexagram ({ hexagram_height, hexagram_width } as model) position hexagram =
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
            |> List.indexedMap (viewLine model)
            |> svg [ width_, height_, x_, y_ ]


viewLine : Model -> Int -> Line -> Svg Message
viewLine { line_height, line_width } position bar =
    let
        y_ : Svg.Attribute Message
        y_ =
            position
                * 2
                |> (*) line_height
                |> toString
                |> y

        line_height_ : Svg.Attribute Message
        line_height_ =
            line_height
                |> toString
                |> height

        line_width_ : Svg.Attribute Message
        line_width_ =
            line_width
                |> toString
                |> width

        broken_line_width : Svg.Attribute Message
        broken_line_width =
            line_width
                // 3
                |> toString
                |> width

        x_offset : Svg.Attribute Message
        x_offset =
            line_width
                // 3
                |> (*) 2
                |> toString
                |> x

        fill_ : Svg.Attribute Message
        fill_ =
            fill "black"

        attributes : List (Svg.Attribute Message)
        attributes =
            [ line_height_, line_width_, y_ ]

        bars : List (Svg Message)
        bars =
            if bar == Unbroken then
                [ rect [ line_width_, line_height_, fill_ ] []
                ]
            else
                [ rect [ broken_line_width, line_height_, fill_ ] []
                , rect [ broken_line_width, line_height_, fill_, x_offset ] []
                ]
    in
        svg attributes bars

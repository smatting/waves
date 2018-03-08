module App exposing (..)
import Html exposing (Html, button, div, text, program)
import Html.Events exposing (onClick, on, onWithOptions)
import Json.Decode as Decode
import Mouse exposing (Position)
import Svg
import Svg.Attributes exposing (..)
import Path.LowLevel as LL exposing (Coordinate)

type alias Lens a b = {
    get : a -> b,
    set : b -> a -> a
}

-- MODEL
type alias Model = {
    topControl2Point : Coordinate,
    topEndpoint2Point : Coordinate,
    drag : Maybe Drag
}


type alias Drag = {
    control : Control,
    controlOrigin : Coordinate,
    start : Position,
    current : Position
}


type Control = TopControl2Drag | TopEndpoint2Drag


init : (Model, Cmd Msg)
init =
    (Model (306.0, 84.0) (377.0, 185.0) Nothing, Cmd.none)


onMouseDown : Control -> Html.Attribute Msg
onMouseDown control =
    let options = { stopPropagation = True , preventDefault = True }
    in onWithOptions "mousedown" options (Decode.map (DragStart control) Mouse.position)


-- VIEW
controlHandle : Control -> Coordinate -> Html.Html Msg
controlHandle control (pointX, pointY) =
    let w = 7 in
    Svg.circle [ cx (toString pointX),
                 cy (toString pointY),
                 r (toString w),
                 onMouseDown control ]
             []

waves : Model -> Html.Html Msg
waves ({topControl2Point, topEndpoint2Point} as model) =
    let
        pathSpec = LL.toString [{moveto =  LL.MoveTo LL.Absolute (49, 255),
                                 drawtos = [LL.CurveTo LL.Absolute [((259, 261), topControl2Point, topEndpoint2Point)]] } ]
    in
        Svg.svg
          [width "800", height "500", viewBox "0 0 800 500"]
          [Svg.path [ d pathSpec, stroke "black", fill "none", strokeWidth "2"] [],
           controlHandle TopControl2Drag topControl2Point,
           controlHandle TopEndpoint2Drag topEndpoint2Point]


view : Model -> Html Msg
view model = div [] [waves model]


-- UPDATE
type Msg
    = DragStart Control Position
    | DragAt Position
    | DragEnd Position


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    (updateHelp msg model, Cmd.none)


applyDrag : Drag -> Lens Model Coordinate -> Model -> Model
applyDrag {control, controlOrigin, start, current} {get, set} model =
    let f = \(originX, originY) start current ->
        (originX + (toFloat (current.x - start.x)),
         originY + (toFloat (current.y - start.y)))
        xy_ = f controlOrigin start current
    in set xy_ model 


coord : Control -> Lens Model Coordinate
coord control =
    case control of
        TopControl2Drag  -> Lens (\model -> model.topControl2Point) (\xy model -> {model | topControl2Point = xy})
        TopEndpoint2Drag -> Lens (\model -> model.topEndpoint2Point) (\xy model -> {model | topEndpoint2Point = xy})


updateHelp : Msg -> Model -> Model
updateHelp msg model =
    case msg of
        DragStart control pos ->
            let xy = ((coord control).get model)
            in {model | drag = Just (Drag control xy pos pos)}

        DragAt pos ->
            case model.drag of
                Nothing    -> model
                Just drag  -> {model | drag = Just {drag | current = pos}}
                              |> applyDrag drag (coord drag.control)
        DragEnd _ ->
            {model | drag = Nothing}


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Sub.none

    Just _ ->
      Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]


-- MAIN
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

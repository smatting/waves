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

type alias Parameter = {
    topControl2Point : Coordinate,
    topEndpoint2Point : Coordinate
}

-- MODEL
type alias Model = {
    param : Parameter,
    drag : Maybe Drag
}


type alias Drag = {
    lens : Lens Parameter Coordinate,
    controlOrigin : Coordinate,
    start : Position,
    current : Position
}




init : (Model, Cmd Msg)
init =
    (Model (Parameter (306.0, 84.0) (377.0, 185.0)) Nothing, Cmd.none)


onMouseDown : Lens Parameter Coordinate -> Html.Attribute Msg
onMouseDown lens =
    let options = { stopPropagation = True , preventDefault = True }
    in onWithOptions "mousedown" options (Decode.map (DragStart lens) Mouse.position)


-- VIEW
controlHandle : Lens Parameter Coordinate -> Parameter -> Html.Html Msg
controlHandle lens param =
    let w = 7
        (pointX, pointY) = lens.get param
    in
    Svg.circle [ cx (toString pointX),
                 cy (toString pointY),
                 r (toString w),
                 onMouseDown lens]
             []

waves : Model -> Html.Html Msg
waves ({param} as model) =
    let
        pathSpec = LL.toString [{moveto =  LL.MoveTo LL.Absolute (49, 255),
                                 drawtos = [LL.CurveTo LL.Absolute [((259, 261), param.topControl2Point, param.topEndpoint2Point)]] } ]
    in
        Svg.svg
          [width "800", height "500", viewBox "0 0 800 500"]
          [Svg.path [ d pathSpec, stroke "black", fill "none", strokeWidth "2"] [],
           controlHandle (Lens (\param -> param.topControl2Point) (\xy param -> {param | topControl2Point = xy})) param,
           controlHandle (Lens (\param -> param.topEndpoint2Point) (\xy param -> {param | topEndpoint2Point = xy})) param]


view : Model -> Html Msg
view model = div [] [waves model]


-- UPDATE
type Msg
    = DragStart (Lens Parameter Coordinate) Position
    | DragAt Position
    | DragEnd Position


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    (updateHelp msg model, Cmd.none)


applyDrag : Drag -> Parameter -> Parameter
applyDrag {lens, controlOrigin, start, current} param =
    let f = \(originX, originY) start current ->
        (originX + (toFloat (current.x - start.x)),
         originY + (toFloat (current.y - start.y)))
        xy = f controlOrigin start current
    in lens.set xy param 


updateHelp : Msg -> Model -> Model
updateHelp msg model =
    case msg of
        DragStart lens pos ->
            let xy = lens.get model.param
            in {model | drag = Just (Drag lens xy pos pos)}

        DragAt pos ->
            case model.drag of
                Nothing    -> model
                Just drag  -> {model | drag  = Just {drag | current = pos},
                                       param = applyDrag drag model.param}

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

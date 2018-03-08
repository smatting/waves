module App exposing (..)
import Html exposing (Html, button, div, text, program)
import Html.Events exposing (onClick, on, onWithOptions, onInput)
import Html.Attributes as H exposing (..)
import Json.Decode as Decode
import Mouse exposing (Position)
import Svg
import Svg.Attributes as S exposing (..)
import Path.LowLevel as LL exposing (Coordinate)

type alias Lens a b = {
    get : a -> b,
    set : b -> a -> a
}

-- MODEL
type alias Parameter = {
    topControl2Point  : Coordinate,
    topEndpoint2Point : Coordinate,
    dummyParam : Float
}

type alias Model = {
    param : Parameter,
    drag  : Maybe Drag
}

type alias Drag = {
    lens         : Lens Parameter Coordinate,
    handleOrigin : Coordinate,
    start        : Position,
    current      : Position
}

init : (Model, Cmd Msg)
init =
    let model = 
        {param = {topControl2Point = (306.0, 84.0),
                  topEndpoint2Point = (377.0, 185.0),
                  dummyParam = 0.5},
         drag  = Nothing}
    in (model, Cmd.none)


-- VIEW
curveHandle : Lens Parameter Coordinate -> Parameter -> Html.Html Msg
curveHandle lens param =
    let w = 7
        (pointX, pointY) = lens.get param
        onMouseDown = 
            let options = { stopPropagation = True , preventDefault = True }
            in onWithOptions "mousedown" options (Decode.map (DragStart lens) Mouse.position)
    in
    Svg.circle [ cx (toString pointX),
                 cy (toString pointY),
                 r (toString w),
                 onMouseDown]
             []

type alias SliderSpec = {
    min   : Float,
    max   : Float,
    step  : Float,
    name  : String
}

sliderView : SliderSpec -> Lens Parameter Float -> Parameter -> Html.Html Msg
sliderView sliderSpec lens param =
    let value = (lens.get param)
    in
        div []
            [text sliderSpec.name
             , Html.input
               [ H.type_ "range"
               , H.min (toString sliderSpec.min)
               , H.max (toString sliderSpec.max)
               , H.step (toString sliderSpec.step)
               , H.value <| toString value
               , onInput (\s -> SliderUpdate lens (Result.withDefault 0.0 (String.toFloat s)))
               ]
               []
             , text (toString value)]


topWave : Coordinate -> Coordinate -> Coordinate -> Coordinate -> Svg.Svg Msg
topWave endPoint1 controlPoint1 controlPoint2 endPoint2 =
    let
        pathSpec = LL.toString [{moveto =  LL.MoveTo LL.Absolute endPoint1,
                                 drawtos = [LL.CurveTo LL.Absolute [(controlPoint1, controlPoint2, endPoint2)]] } ]
    in Svg.path [ d pathSpec, stroke "black", fill "none", strokeWidth "1.5"] []


waves : Model -> Html.Html Msg
waves ({param} as model) =
    let range = List.map (\i -> toFloat i) (List.range 0 5)
        ws = List.map (\i -> topWave (49, 255 + i * param.dummyParam) (259, 261) param.topControl2Point param.topEndpoint2Point) range
        handles = [curveHandle (Lens (\param -> param.topControl2Point) (\xy param -> {param | topControl2Point = xy})) param,
                   curveHandle (Lens (\param -> param.topEndpoint2Point) (\xy param -> {param | topEndpoint2Point = xy})) param]
    in Svg.svg
           [S.width "800", S.height "500", S.viewBox "0 0 800 500"]
           (ws ++ handles)
      

controls : Parameter -> Html.Html Msg
controls param =
    div [] [sliderView {min = 5, max = 50, step = 1, name = "dummyParam"}
                       (Lens (\param -> param.dummyParam) (\value param -> {param | dummyParam = value}))
                       param]


view : Model -> Html Msg
view model = div [] [waves model, controls model.param]

-- UPDATE
type Msg
    = DragStart (Lens Parameter Coordinate) Position
    | DragAt Position
    | DragEnd Position
    | SliderUpdate (Lens Parameter Float) Float


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    (updateHelp msg model, Cmd.none)


applyDrag : Drag -> Parameter -> Parameter
applyDrag {lens, handleOrigin, start, current} param =
    let f = \(originX, originY) start current ->
        (originX + (toFloat (current.x - start.x)),
         originY + (toFloat (current.y - start.y)))
        xy = f handleOrigin start current
    in lens.set xy param 


updateHelp : Msg -> Model -> Model
updateHelp msg model =
    case msg of
        DragStart lens pos ->
            let handleOrigin = lens.get model.param
            in {model | drag = Just (Drag lens handleOrigin pos pos)}

        DragAt pos ->
            case model.drag of
                Nothing    -> model
                Just drag  -> {model | drag  = Just {drag | current = pos},
                                       param = applyDrag drag model.param}

        DragEnd _ ->
            {model | drag = Nothing}

        SliderUpdate lens value ->
            let param2 = lens.set value model.param
            in {model | param = param2}


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

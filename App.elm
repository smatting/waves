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
    topControlPoint1  : Coordinate,
    topControlPoint2  : Coordinate,
    topEndpointPoint2 : Coordinate,

    topEndPoint1dy : Float,
    topControlPoint1dy : Float,
    topControlPoint1dx : Float,
    topControlPoint2dy : Float,
    topControlPoint2dx : Float,
    topEndPoint2dx : Float,
    topEndPoint2dy : Float,

    bottomEndPoint1 : Coordinate,
    bottomControlPoint1 : Coordinate,
    bottomControlPoint2 : Coordinate,
    bottomEndPoint1dx : Float,
    bottomEndPoint1dy : Float,
    bottomControlPoint1dy : Float,
    bottomControlPoint1dx : Float,
    bottomControlPoint2dy : Float,
    bottomControlPoint2dx : Float,
    bottomEndPoint2dy : Float
}

type alias Model = {
    param : Parameter,
    drag  : Maybe Drag,
    showHandles : Bool
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
        {param = {
            topControlPoint1 = (259.0, 261.0),
            topControlPoint2 = (306.0, 84.0),
            topEndpointPoint2 = (377.0, 185.0),

            topEndPoint1dy = 25,
            topControlPoint1dx = 0,
            topControlPoint1dy = 25,
            topControlPoint2dx = -12,
            topControlPoint2dy = 25,
            topEndPoint2dx = -13,
            topEndPoint2dy = 20,

            bottomEndPoint1 = (400.0, 200.0),
            bottomControlPoint1 = (500.0, 450.0),
            bottomControlPoint2 = (700.0, 250.0),

            bottomEndPoint1dx = -10,
            bottomEndPoint1dy = 22,
            bottomControlPoint1dx = 3,
            bottomControlPoint1dy = 17,
            bottomControlPoint2dx = 50,
            bottomControlPoint2dy = 25,
            bottomEndPoint2dy = 25
         },
         drag  = Nothing,
         showHandles = True}
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


wave : Coordinate -> Coordinate -> Coordinate -> Coordinate -> Svg.Svg Msg
wave endPoint1 controlPoint1 controlPoint2 endPoint2 =
    let
        pathSpec = LL.toString [{moveto =  LL.MoveTo LL.Absolute endPoint1,
                                 drawtos = [LL.CurveTo LL.Absolute [(controlPoint1, controlPoint2, endPoint2)]] } ]
    in Svg.path [ d pathSpec, stroke "black", fill "none", strokeWidth "3", strokeLinecap "round"] []


addDelta : Coordinate -> Float -> Coordinate -> Coordinate
addDelta (startX, startY) s (deltaX, deltaY) =
    (startX + s * deltaX, startY + s * deltaY)


isNothing : Maybe a -> Bool
isNothing m =
    case m of
        Nothing -> True
        Just _ -> False


waves : Model -> Html.Html Msg
waves ({param, showHandles} as model) =
    let range = List.map (\i -> toFloat i) (List.range 0 5)
        topWaves = List.map (\i -> wave
                                       (addDelta (0, 250) i (0.0, param.topEndPoint1dy))
                                       (addDelta param.topControlPoint1 i (param.topControlPoint1dx, param.topControlPoint1dy))
                                       (addDelta param.topControlPoint2 i (param.topControlPoint2dx, param.topControlPoint2dy))
                                       (addDelta param.topEndpointPoint2 i (param.topEndPoint2dx, param.topEndPoint2dy)))
                   range
        bottomWaves = List.map (\i -> wave
                                          (addDelta param.bottomEndPoint1 i (param.bottomEndPoint1dx, param.bottomEndPoint1dy))
                                          (addDelta param.bottomControlPoint1 i (param.bottomControlPoint1dx, param.bottomControlPoint1dy))
                                          (addDelta param.bottomControlPoint2 i (param.bottomControlPoint2dx, param.bottomControlPoint2dy))
                                          (addDelta (1000, 250) i (0.0, param.bottomEndPoint2dy)))
                      range
        topHandles = [curveHandle (Lens (\param -> param.topControlPoint2) (\xy param -> {param | topControlPoint2 = xy})) param,
                      curveHandle (Lens (\param -> param.topEndpointPoint2) (\xy param -> {param | topEndpointPoint2 = xy})) param,
                      curveHandle (Lens (\param -> param.topControlPoint1) (\xy param -> {param | topControlPoint1 = xy})) param]
        bottomHandles = [curveHandle (Lens (\param -> param.bottomEndPoint1) (\xy param -> {param | bottomEndPoint1 = xy})) param,
                         curveHandle (Lens (\param -> param.bottomControlPoint1) (\xy param -> {param | bottomControlPoint1 = xy})) param,
                         curveHandle (Lens (\param -> param.bottomControlPoint2) (\xy param -> {param | bottomControlPoint2 = xy})) param
                         ]
        handles = if showHandles && (isNothing model.drag) then topHandles ++ bottomHandles else []
    in Svg.svg
           [S.width "1200", S.height "500", S.viewBox "0 0 1200 500"]
           (topWaves ++ bottomWaves ++ handles)
      

topControls : Parameter -> Html.Html Msg
topControls param =
    div []
    [sliderView {min = 5, max = 50, step = 1, name = "topEndPoint1dy"}
                (Lens (\param -> param.topEndPoint1dy) (\value param -> {param | topEndPoint1dy = value}))
                param,

     sliderView {min = -50, max = 50, step = 1, name = "topControlPoint1dx"}
                (Lens (\param -> param.topControlPoint1dx) (\value param -> {param | topControlPoint1dx = value}))
                param,
     sliderView {min = 5, max = 50, step = 1, name = "topControlPoint1dy"}
                (Lens (\param -> param.topControlPoint1dy) (\value param -> {param | topControlPoint1dy = value}))
                param,

     sliderView {min = -50, max = 50, step = 1, name = "topControlPoint2dx"}
                (Lens (\param -> param.topControlPoint2dx) (\value param -> {param | topControlPoint2dx = value}))
                param,
     sliderView {min = 5, max = 50, step = 1, name = "topControlPoint2dy"}
                (Lens (\param -> param.topControlPoint2dy) (\value param -> {param | topControlPoint2dy = value}))
                param,

     sliderView {min = -50, max = 50, step = 1, name = "topEndPoint2dx"}
                (Lens (\param -> param.topEndPoint2dx) (\value param -> {param | topEndPoint2dx = value}))
                param,
     sliderView {min = 5, max = 50, step = 1, name = "topEndPoint2dy"}
                (Lens (\param -> param.topEndPoint2dy) (\value param -> {param | topEndPoint2dy = value}))
                param
    ]

bottomControls : Parameter -> Html.Html Msg
bottomControls param =
    div []
    [sliderView {min = -50, max = 50, step = 1, name = "bottomEndPoint1dx"}
                (Lens (\param -> param.bottomEndPoint1dx) (\value param -> {param | bottomEndPoint1dx = value}))
                param,
     sliderView {min = 5, max = 50, step = 1, name = "bottomEndPoint1dy"}
                (Lens (\param -> param.bottomEndPoint1dy) (\value param -> {param | bottomEndPoint1dy = value}))
                param,

     sliderView {min = -50, max = 50, step = 1, name = "bottomControlPoint1dx"}
                (Lens (\param -> param.bottomControlPoint1dx) (\value param -> {param | bottomControlPoint1dx = value}))
                param,
     sliderView {min = 5, max = 50, step = 1, name = "bottomControlPoint1dy"}
                (Lens (\param -> param.bottomControlPoint1dy) (\value param -> {param | bottomControlPoint1dy = value}))
                param,
     sliderView {min = -50, max = 50, step = 1, name = "bottomControlPoint2dx"}
                (Lens (\param -> param.bottomControlPoint2dx) (\value param -> {param | bottomControlPoint2dx = value}))
                param,
     sliderView {min = 5, max = 50, step = 1, name = "bottomControlPoint2dy"}
                (Lens (\param -> param.bottomControlPoint2dy) (\value param -> {param | bottomControlPoint2dy = value}))
                param,
     sliderView {min = 5, max = 50, step = 1, name = "bottomEndPoint2dy"}
                (Lens (\param -> param.bottomEndPoint2dy) (\value param -> {param | bottomEndPoint2dy = value}))
                param
     ]
            

checkbox : msg -> String -> Html msg
checkbox msg name =
        Html.label
        [ H.style [("padding", "20px")]
        ]
        [ Html.input [ H.type_ "checkbox", onClick msg,  H.checked True] []
        , text name
        ]


view : Model -> Html Msg
view model = div [] [waves model,
                     checkbox ToggleHandles "toggle handles",
                     topControls model.param,
                     bottomControls model.param]

-- UPDATE
type Msg
    = DragStart (Lens Parameter Coordinate) Position
    | DragAt Position
    | DragEnd Position
    | SliderUpdate (Lens Parameter Float) Float
    | ToggleHandles


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

        ToggleHandles -> {model | showHandles = not model.showHandles}


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

module App exposing (..)

import Html exposing (Html, button, div, text, program)
import Html.Events exposing (onClick, on, onWithOptions)
import Json.Decode as Decode
import Mouse exposing (Position)

import Svg exposing (..)
import Svg.Attributes exposing (..)

import Path.LowLevel as LL exposing (..)

-- MODEL


type alias Model = {
    topEndpoint1Pos : Position,
    drag : Maybe Drag
    }


type alias Drag = { dragged : Dragged, current : Position }

type Dragged = DragTopEndpoint1

init : ( Model, Cmd Msg )
init =
    ( Model (Position 306 84) Nothing, Cmd.none )


-- VIEW
dragButton : Position -> Html.Html Msg
dragButton pos =
    let w = 30
    in
    rect [ x (Basics.toString (pos.x - w // 2)),
           y (Basics.toString (pos.y - w // 2)),
           width (Basics.toString w),
           height (Basics.toString w),
           onMouseDown DragTopEndpoint1 ]
         []

onMouseDown : Dragged -> Attribute Msg
onMouseDown dragged =
    let options = { stopPropagation = True , preventDefault = True }
    in onWithOptions "mousedown" options (Decode.map (DragStart dragged) Mouse.position)

waves : Model -> Html.Html Msg
waves ({topEndpoint1Pos} as model) =
    let
        pathSpec = LL.toString [ { moveto =  MoveTo Absolute (49, 255),
                                   drawtos = [ CurveTo Absolute [ ((259, 261), (toFloat topEndpoint1Pos.x, toFloat topEndpoint1Pos.y), (377, 185)) ] ] } ] 
    in
        svg
          [ width "500", height "500", viewBox "0 0 500 500" ]
          [ Svg.path [ d pathSpec, stroke "black", fill "none", strokeWidth "2" ] [],
            dragButton model.topEndpoint1Pos ]


view : Model -> Html Msg
view model = waves model


-- UPDATE
type Msg
    = DragStart Dragged Position
    | DragAt Position
    | DragEnd Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( updateHelp msg model, Cmd.none )


setPosition : Dragged -> Position -> Model -> Model
setPosition dragged =
    case dragged of
        DragTopEndpoint1 -> (\pos model -> { model | topEndpoint1Pos = pos })

setDrag : Maybe Drag -> Model -> Model
setDrag maybeDrag model = { model | drag = maybeDrag }


updateHelp : Msg -> Model -> Model
updateHelp msg model =
    case msg of
        DragStart dragged pos ->
            model |> setDrag (Just (Drag dragged pos))
                  |> setPosition dragged pos

        DragAt pos ->
            let {drag} = model in
            case drag of
                Nothing -> model
                Just drag_ -> model |> setDrag (Just (Drag drag_.dragged pos))
                                    |> setPosition drag_.dragged pos
        DragEnd _ ->
            model |> setDrag Nothing


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

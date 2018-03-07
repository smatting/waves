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


type alias Drag =
    {
      dragged : Dragged,
      current : Position
    }

type Dragged = DragTopEndpoint1


init : ( Model, Cmd Msg )
init =
    ( Model (Position 306 84) Nothing, Cmd.none )


-- VIEW
dragButton : Dragged -> Position -> Html.Html Msg
dragButton dragged pos =
    let w = 30
    in
    rect [ x (Basics.toString (pos.x - w // 2)),
           y (Basics.toString (pos.y - w // 2)),
           width (Basics.toString w),
           height (Basics.toString w),
           onMouseDown dragged]
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
            dragButton DragTopEndpoint1 (model.topEndpoint1Pos) ]


view : Model -> Html Msg
view model = waves model


-- UPDATE
type Msg
    = DragStart Dragged Position
    | DragAt Dragged Position
    | DragEnd Dragged Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( updateHelp msg model, Cmd.none )


updateHelp : Msg -> Model -> Model
updateHelp msg ({topEndpoint1Pos, drag} as model) =
  case msg of
    DragStart dragged pos ->
        Model (getPosition model) (Just (Drag dragged pos))

    DragAt dragged pos ->
        case drag of
            Nothing -> model
            Just drag2 -> Model (getPosition model) (Just (Drag dragged pos))

        -- {model | drag = Maybe.map (\{start} -> Drag dragged start pos) drag}
    DragEnd dragged _ ->
      Model (getPosition model) Nothing


getPosition : Model -> Position
getPosition {topEndpoint1Pos, drag} =
  case drag of
    Nothing -> topEndpoint1Pos

    Just {current} -> current
      -- Position
      --   (topEndpoint1Pos.x + current.x - start.x)
      --   (topEndpoint1Pos.y + current.y - start.y)



-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Sub.none

    Just _ ->
      Sub.batch [ Mouse.moves (DragAt DragTopEndpoint1), Mouse.ups (DragEnd DragTopEndpoint1) ]


-- MAIN
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

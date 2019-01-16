
module Slider exposing
  ( Data
  , Drag
  , Model
  , Msg(..)
  , Orientation(..)
  , Rendering
  , getScaledMarkerOffset
  , initData
  , initDrag
  , initModel
  , initRendering
  , interactionEvents
  , setDataMaxBound
  , setDataMinBound
  , setDataOffset
  , setDataStartOffset
  , setDragCurrent
  , setDragStart
  , setModelData
  , setModelDrag
  , setModelRendering
  , setRenderingOrientation
  , setRenderingPosition
  , setRenderingSize
  , subscriptions
  , update
  )

import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as Decode
import Mouse exposing (Position)
import SingleTouch
import Touch exposing (Coordinates, clientPos)


-- MODEL


type alias Model =
    { rendering : Rendering
    , data      : Data
    , drag      : Maybe Drag
    }


setModelRendering : Rendering -> Model -> Model
setModelRendering x model =
  { model | rendering = x }


setModelData : Data -> Model -> Model
setModelData x model =
  { model | data = x }


setModelDrag : Maybe Drag -> Model -> Model
setModelDrag x model =
  { model | drag = x }


initModel : Model
initModel =
  Model initRendering initData Nothing


type alias Rendering =
    { position    : Position
    , size        : Int
    , orientation : Orientation
    }


type Orientation
  = Horizontal
  | Vertical


setRenderingPosition : Position -> Rendering -> Rendering
setRenderingPosition x rendering =
  { rendering | position = x }


setRenderingSize : Int -> Rendering -> Rendering
setRenderingSize x rendering =
  { rendering | size = x }


setRenderingOrientation : Orientation -> Rendering -> Rendering
setRenderingOrientation x rendering =
  { rendering | orientation = x }


initRendering : Rendering
initRendering =
  Rendering (Position 0 0) 0 Horizontal


type alias Data =
    { minBound    : Int
    , maxBound    : Int
    , offset      : Int
    , startOffset : Int
    }


setDataMinBound : Int -> Data -> Data
setDataMinBound x data =
  { data | minBound = x }


setDataMaxBound : Int -> Data -> Data
setDataMaxBound x data =
  { data | maxBound = x }


setDataOffset : Int -> Data -> Data
setDataOffset x data =
  { data | offset = x }


setDataStartOffset : Int -> Data -> Data
setDataStartOffset x data =
  { data | startOffset = x }


initData : Data
initData =
  Data 0 0 0 0


type alias Drag =
    { start   : Position
    , current : Position
    }


setDragStart : Position -> Drag -> Drag
setDragStart x drag =
  { drag | start = x }


setDragCurrent : Position -> Drag -> Drag
setDragCurrent x drag =
  { drag | current = x }


initDrag : Drag
initDrag =
  Drag (Position 0 0) (Position 0 0)


-- UPDATE


type Msg
    = MouseDragStart Position
    | MouseDragAt Position
    | MouseDragEnd Position
    | SetOffset Int Position
    | TouchDragStart Coordinates
    | TouchDragAt Coordinates
    | TouchDragEnd Coordinates


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    MouseDragStart xy ->
      ( model
          |> setModelData (model.data |> setDataStartOffset model.data.offset)
          |> setModelDrag (Just (Drag xy xy))
      , Cmd.none
      )

    MouseDragAt xy ->
      ( model
          |> setModelData (model.data |> setDataOffset (getBoundedPosition model))
          |> setModelDrag (Maybe.map (\{start} -> Drag start xy) model.drag)
      , Cmd.none
      )

    MouseDragEnd _ ->
      ( model
          |> setModelDrag Nothing
      , Cmd.none
      )

    SetOffset adjustment xy ->
      ( model
          |> setModelData (model.data |> setDataOffset (getOffsetFromPoint model xy adjustment))
          |> setModelDrag Nothing
      , Cmd.none
      )
    
    TouchDragStart xy ->
      ( model
          |> setModelData (model.data |> setDataStartOffset model.data.offset)
          |> setModelDrag (Just (Drag (coordsToPos xy) (coordsToPos xy)))
      , Cmd.none
      )

    TouchDragAt xy ->
      ( model
          |> setModelData (model.data |> setDataOffset (getBoundedPosition model))
          |> setModelDrag (Maybe.map (\{start} -> Drag start (coordsToPos xy)) model.drag)
      , Cmd.none
      )

    TouchDragEnd _ ->
      ( model
          |> setModelDrag Nothing
      , Cmd.none
      )


coordsToPos : Coordinates -> Position
coordsToPos xs =
  clientPos xs
    |> (\(x, y) -> Position (Basics.round x) (Basics.round y))


getBoundedPosition : Model -> Int
getBoundedPosition model =
  case model.drag of
    Nothing ->
      model.data.offset

    Just drag ->
      let
        scalePerc = toFloat (model.data.maxBound - model.data.minBound) / toFloat model.rendering.size
        dragChange =
          case model.rendering.orientation of
            Horizontal -> floor (toFloat (drag.current.x - drag.start.x) * scalePerc)
            Vertical   -> floor (toFloat (drag.current.y - drag.start.y) * scalePerc)
      in
        Basics.min (Basics.max (model.data.startOffset + dragChange) model.data.minBound) model.data.maxBound


getOffsetFromPoint : Model -> Position -> Int -> Int
getOffsetFromPoint model { x, y } adjustment =
  let
    unscaledOffset =
      case model.rendering.orientation of
        Horizontal -> x - model.rendering.position.x
        Vertical   -> y - model.rendering.position.y
    scalePerc =
      toFloat (model.data.maxBound - model.data.minBound) / toFloat model.rendering.size
  in
    floor (toFloat unscaledOffset * scalePerc) + adjustment


{-| Gets the offset position at which the marker should be rendered. -}
getScaledMarkerOffset : Rendering -> Data -> Int
getScaledMarkerOffset rendering data =
  let
    offsetPerc = toFloat (data.offset - data.minBound) / toFloat (data.maxBound - data.minBound)
    scalePerc = toFloat (data.maxBound - data.minBound) / toFloat rendering.size
  in
    floor (toFloat rendering.size * offsetPerc)


interactionEvents : List (Attribute Msg)
interactionEvents =
    [ SingleTouch.onStart TouchDragStart
    , SingleTouch.onMove TouchDragAt
    , SingleTouch.onEnd TouchDragEnd
    , on "mousedown" (Decode.map MouseDragStart Mouse.position)
    ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Sub.none

    Just _ ->
      Sub.batch
        [ Mouse.moves MouseDragAt
        , Mouse.ups MouseDragEnd
        ]


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Decode
import Mouse exposing (Position)

import Slider


-- VIEW


(=>) : a -> b -> ( a, b )
(=>) = (,)


view : Slider.Model -> Html Slider.Msg
view model =
  div []
    [ renderMarker model
    , renderBar model
    , div
       [ style
          [ "position" => "absolute"
          , "left" => px (model.rendering.position.x + 8)
          , "top" => px (model.rendering.position.y + 45)
          ]
       ]
       [ text (toString model.data.offset) ]
    ]


renderMarker : Slider.Model -> Html Slider.Msg
renderMarker model =
  let
    scaledMarkerPosition = Slider.getScaledMarkerOffset model.rendering model.data
  in
    case model.rendering.orientation of
      Slider.Horizontal ->
        div []
          [ div
            [ dragOnMouseDown
            , style
              [ "background-color" => "#999999"
              , "width" => "16px"
              , "height" => "30px"
              , "border-top-left-radius" => "4px"
              , "border-top-right-radius" => "4px"
              , "position" => "relative"
              , "left" => px (model.rendering.position.x + scaledMarkerPosition)
              , "top" => px model.rendering.position.y
              , "z-index" => "1"
              ]
            ] []
          , div
            [ dragOnMouseDown
            , style
              [ "width" => "0"
              , "height" => "0"
              , "border-left" => "8px solid transparent"
              , "border-right" => "8px solid transparent"
              , "border-top" => "8px solid #999999"
              , "position" => "relative"
              , "left" => px (model.rendering.position.x + scaledMarkerPosition)
              , "top" => px model.rendering.position.y
              , "z-index" => "1"
              ]
            ] []
          ]
      Slider.Vertical ->
        div []
          [ div
            [ dragOnMouseDown
            , style
              [ "background-color" => "#999999"
              , "width" => "30px"
              , "height" => "16px"
              , "border-top-left-radius" => "4px"
              , "border-bottom-left-radius" => "4px"
              , "position" => "relative"
              , "left" => px (model.rendering.position.x - 27)
              , "top" => px (model.rendering.position.y + scaledMarkerPosition + 27)
              , "z-index" => "1"
              ]
            ] []
          , div
            [ dragOnMouseDown
            , style
              [ "width" => "0"
              , "height" => "0"
              , "border-top" => "8px solid transparent"
              , "border-bottom" => "8px solid transparent"
              , "border-left" => "8px solid #999999"
              , "position" => "relative"
              , "left" => px (model.rendering.position.x + 3)
              , "top" => px (model.rendering.position.y + scaledMarkerPosition + 11)
              , "z-index" => "1"
              ]
            ] []
          ]


renderBar : Slider.Model -> Html Slider.Msg
renderBar model =
  let
    scaledMarkerPosition = Slider.getScaledMarkerOffset model.rendering model.data
  in
    case model.rendering.orientation of
      Slider.Horizontal ->
        div
          []
          [ span
            [ setOnMouseDown
            , style
              [ "width" => px model.rendering.size
              , "height" => "5px"
              , "background-color" => "#cccccc"
              , "position" => "absolute"
              , "left" => px (model.rendering.position.x + 8)
              , "top" => px (model.rendering.position.y + 35)
              , "border-radius" => "2px"
              ]
            ] []
          , span
            [ setOnMouseDown
            , style
              [ "width" => px scaledMarkerPosition
              , "height" => "5px"
              , "background-color" => "#3366ff"
              , "position" => "absolute"
              , "left" => px (model.rendering.position.x + 8)
              , "top" => px (model.rendering.position.y + 35)
              , "border-radius" => "2px"
              ]
            ] []
          ]
      Slider.Vertical ->
        div
          []
          [ span
            [ setOnMouseDown
            , style
              [ "width" => "5px"
              , "height" => px model.rendering.size
              , "background-color" => "#cccccc"
              , "position" => "absolute"
              , "left" => px (model.rendering.position.x + 8)
              , "top" => px (model.rendering.position.y + 35)
              , "border-radius" => "2px"
              ]
            ] []
          , span
            [ setOnMouseDown
            , style
              [ "width" => "5px"
              , "height" => px scaledMarkerPosition
              , "background-color" => "#3366ff"
              , "position" => "absolute"
              , "left" => px (model.rendering.position.x + 8)
              , "top" => px (model.rendering.position.y + 35)
              , "border-radius" => "2px"
              ]
            ] []
          ]


px : Int -> String
px number =
  toString number ++ "px"


dragOnMouseDown : Attribute Slider.Msg
dragOnMouseDown =
  on "mousedown" (Decode.map Slider.DragStart Mouse.position)


setOnMouseDown : Attribute Slider.Msg
setOnMouseDown =
  -- -8 for Horizontal orientation, -36 for Vertical
  on "mousedown" (Decode.map (Slider.SetOffset -8) Mouse.position)


init : ( Slider.Model, Cmd Slider.Msg )
init =
  ( Slider.initModel
      |> Slider.setModelRendering ( Slider.Rendering (Position 200 200) 100 Slider.Horizontal )
      |> Slider.setModelData ( Slider.Data 0 5 0 0 )
  , Cmd.none
  )


main =
  Html.program
    { init = init
    , view = view
    , update = Slider.update
    , subscriptions = Slider.subscriptions
    }

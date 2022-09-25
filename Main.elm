module Main exposing (main)
import Browser
import Html exposing (Html)
import Dict exposing (Dict)
import Svg exposing (svg)
import Svg.Attributes as SAttr
import Time exposing (Posix)

type Msg
  = Tick Posix

type alias Position = (Int, Int)

type Direction
  = N | E | W | S

type Color
  = White
  | Black

type alias Model =
  { field : Dict Position Color
  , currentPos : Position
  , currentDirection : Direction
  }

speed = 4

-- INIT

init =
  { field =
      List.range 0 120
        |> List.map
          (\x ->
            List.range 0 120
              |> List.map
                (\y ->
                  (x,y)
                )
          )
        |> List.concat
        |> List.map (\pos -> (pos, White))
        |> Dict.fromList
  , currentPos = (59,59)
  , currentDirection = W
  }

update msg model =
  case msg of
    Tick _ ->
      repeatStep speed model

repeatStep n model =
  if n == 0
  then model
  else repeatStep (n-1) (step model)

step model =
  case Dict.get model.currentPos model.field of
    Just col ->
      { field = Dict.insert model.currentPos (changeColor col) model.field
      , currentPos = changePos col model.currentDirection model.currentPos
      , currentDirection = changeDirection col model.currentDirection
      }

    _ -> model

changeColor col =
  case col of
    White -> Black
    Black -> White

changePos col dir (x,y) =
  case dir of
    N -> (x-col2int col |> modBy 121, y)
    S -> (x+col2int col |> modBy 121, y)

    E -> (x, y-col2int col |> modBy 121)
    W -> (x, y+col2int col |> modBy 121)


changeDirection col dir =
  dir2int dir + col2int col
    |> modBy 4
    |> int2dir

dir2int dir =
  case dir of
    N -> 0
    E -> 3
    W -> 1
    S -> 2

int2dir n =
  case n of
    1 -> W
    2 -> S
    3 -> E
    _ -> N

col2int col =
  case col of
    White -> -1
    Black -> 1

col2str col =
  case col of
    White -> "white"
    Black -> "black"

view model =
  let
    createRect (x,y) col =
      Svg.rect
        [ SAttr.x <| i2s (x*10)
        , SAttr.y <| i2s (y*10)
        , SAttr.width "10"
        , SAttr.height "10"
        , SAttr.stroke "black"
        , SAttr.strokeWidth "1"
        , SAttr.fill <| col2str col
        ][]

    viewList =
      model.field
        |> Dict.toList
        |> List.map
          (\(pos, color) -> createRect pos color)

    current (x,y) =
      Svg.rect
        [ SAttr.x <| i2s (x*10)
        , SAttr.y <| i2s (y*10)
        , SAttr.width "10"
        , SAttr.height "10"
        , SAttr.stroke "black"
        , SAttr.strokeWidth "1"
        , SAttr.fill "red"
        ][]
      |> List.singleton
  in
    svg
      [ SAttr.width  "1200"
      , SAttr.height "1200"
      , SAttr.viewBox "0 0 1200 1200"
      ]
      (viewList ++ current model.currentPos)

i2s = String.fromInt

-- MAIN

main : Program () Model Msg
main =
  Browser.element
    { init = \_ -> (init, Cmd.none)
    , view = view
    , update = \msg model -> ( update msg model, Cmd.none )
    , subscriptions = \_ -> Time.every 16 Tick
    }
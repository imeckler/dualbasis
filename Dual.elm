module Dual where

import Debug
import Color
import Signal
import Html
import Html(..)
import Html.Events(..)
import Html.Attributes(..)
import DragAndDrop as DD
import Graphics.Collage(..)
import Graphics.Element(..)

hoverChan : Signal.Channel (Int, Bool)
hoverChan = Signal.channel (0, False)

type alias Point = (Float, Float)

-- type alias State = { drags : (Point, Point), duals : (Point

norm (x, y) = sqrt (x^2 + y^2)

px n = toString n ++ "px"

handle : Int -> Point -> Form
handle i pos =
  let (w, h) = (10, 10) in
  div
  [ onMouseEnter (Signal.send hoverChan (i, True)), onMouseLeave (Signal.send hoverChan (i, False))
  , style
    [ ("width", px w), ("height", px h)
    , ("borderRadius", px (w / 2))
--    , ("backgroundColor", "blue")
    ]
  ] []
  |> Html.toElement w h
  |> toForm
  |> move pos

actionSignal i =
  DD.track False (Signal.map snd <| Signal.keepIf (((==) i) << fst) (0, False) <| Signal.subscribe hoverChan)

integrate i pos0 =
  actionSignal i |>
  Signal.foldp (\a (x, y) -> case a of
    Just (DD.MoveBy (dx, dy)) -> (x + dx, y - dy)
    _ -> (x, y))
    pos0

dragPoint1 = Signal.map (both toFloat) <| integrate 1 (100, 0)
dragPoint2 = Signal.map (both toFloat) <| integrate 2 (0, 100)

drawVector v col =
  let (x, y) = v
      d      = norm v
  in
  group
  [ traced (solid col) (segment (0, 0) (d, 0))
  , ngon 3 5 |> filled col |> moveX d
  ]
  |> rotate (atan2 y x)

w = 500
h = 500

draw dragPt1 dragPt2 =
  let (dual1, dual2) = both (both ((*) 100)) <| dualBasis (both (\x -> x /100) dragPt1) (both (\x -> x / 100) dragPt2)
      axes =
        let mk (a, b) = traced (dotted Color.black) (segment (-a, -b) (a, b)) in
        group [mk (w/2, 0), mk (0, h/2)]
  in
  group
  [ axes
  , drawVector dual1 Color.red
  , drawVector dual2 Color.red
  , drawVector dragPt1 Color.black
  , drawVector dragPt2 Color.black
  , handle 1 dragPt1
  , handle 2 dragPt2
  ]


dualBasis (x1, y1) (x2, y2) =
  let d = x2 * y1 - x1 * y2
  in
  if d == 0
  then ((0, 0), (0, 0))
  else ((y1 / d, -x1 / d), (-y2 / d, x2 / d))

both f (x, y) = (f x, f y)

main = Signal.map2 (\v1 v2 -> collage w h [draw v1 v2]) dragPoint1 dragPoint2

module Web exposing (..)
{-| css stylings
@docs wallStyle
@docs wallBorderStyle
@docs robotWrapper
@docs boardWrapper
@docs markerWrapper
@docs markerImage
@docs markerStyle
@docs wallWrapper
@docs robotImage
@docs put
-}

import Tuple exposing (first , second)
import Common exposing (..)
import String exposing (concat)
import Board exposing (..)
import Marker exposing (..)
import Robot exposing (..)
import Html exposing (Html, div, text, Attribute, button, img)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, class, src, classList)

type Msg
  = Move Move
  | NewGame Original
  | Start
  | Restart
  | NewMarker Marker
  | NextMarker

type alias Move = (Robot, Direction)

type alias Original = {b: Board, r: List Robot, m: List Marker, gm: Marker}

{-| Adds styling to put a element in a grid -}
put : Int -> Int -> List (String, String)
put x y = [("grid-column", toString (x+1)), ("grid-row", toString (y+1))]

{-| Given a color returns the correct image -}
robotImage : Color -> String
robotImage c = concat ["media/", (toString c), "/Robot.svg"]

{-| Given a color returns the correct image -}
markerImage : Color -> Symbol -> String
markerImage c s = concat ["media/", (toString c), "/", (toString s), ".svg"]

showBoard : Model -> List (Html Msg)
showBoard m = let
    board = m.og.b
    size = board.s
    lPos = allPos size
    robots = m.og.r
    markers = m.og.m
  in
    List.map (makeCell robots markers board) lPos

makeCell : List Robot -> List Marker -> List Wall -> Pos -> Html Msg
makeCell lr lm lw p = let 
    (robotClass, robotElems) = makeRobot p lr
    (wallClass, wallIndex) = makeWall pos lw 1 -- The one is to initate an iterator
    (markerClass, markerImg) = makeMarker p lm lw wallIndex
  in 
    div [String.join " " [ robotClass, markerClass, wallClass, style <| put (first p) (second p)] ] [ markerImg :: robotElems ]

makeRobot : Pos -> List Robot -> (String, List (Html Msg))
makeRobot p lr = case List.head lr of
  Just r -> case r.p of
    p -> ("robot-cell" [robotCell r])
    _ -> makeRobot pos <|  List.drop 1 lr
  Nothing -> ("", [])

robotCell : Robot -> List (Html Msg)
robotCell r = 
 [ img [
          src "media/Up.svg",
          onClick (Move (r, N)),
          classList [
                      ("btn-dir", True),
                      ("btn-dir-north", True)
                    ]
        ] [],
    img [
          src "media/Left.svg",
          onClick (Move (r, W)),
          classList [
                      ("btn-dir", True),
                      ("btn-dir-west", True)
                    ]
        ] [],
    img [
          src "media/Right.svg",
          onClick (Move (r, E)),
          classList [
                      ("btn-dir", True),
                      ("btn-dir-east", True)
                    ]
        ] [],
    img [
          src "media/Down.svg",
          onClick (Move (r, S)),
          classList [
                      ("btn-dir", True),
                      ("btn-dir-south", True)
                    ]
        ] [],
    img [
          src <| robotImage r.c,
          class "robot"
        ] []
  ]

makeWall : Pos -> List Wall -> Int -> (String, Int)
makeWall p lw i = case lw of 
  w::lw -> if first w == p || second w == p
            then (wallClass p w, i)
            else makeWall p lw (i+1)
  [] -> ("", (-1))

wallClass : Pos -> Wall -> String
wallClass p (pw1, pw2) = if pw1 == p
  then wallFromTo pw1 pw2
  else wallFromTo pw2 pw1

wallFromTo : Pos -> Pos -> String
wallFromTo (x1, y1) (x2, y2) = case (x2 - x1, y2 - y1) of
  (0, 1)  -> "north-wall" 
  (0, -1) -> "south-wall"
  (1, 0)  -> "east-wall"
  (-1, 0) -> "west-wall"

makeMarker : Pos -> List Marker -> List Wall -> Int -> (String, List (Html Msg))
makeMarker pos lm lw wallIndex = case lookUpMarkerWith wallIndex lm of
  Just m -> 
    let
      (p1, p2) = getAt m.i lw
    in
      if m.r == 0 
      then 
        if pos == p1 
        then img [src <| markerImage m.c m.s, class "marker"][]
        else ("", [])
      else 
        if pos == p2
        then img [src <| markerImage m.c m.s, class "marker"][]
        else ("", [])        
  Nothing -> ("", [])

lookUpMarkerWith : Int -> List Marker -> Maybe Marker
lookUpMarkerWith i lm = case (i, lm) of
   ((-1), _) -> Nothing
   (i, m::lm) -> lookUpMarkerWith (i-1) lm
   (0, m::lm) -> Just m
   (_, []) -> Nothing

{-| Generate html code for the goal marker -}
showGoalMarker : Marker -> Html Msg
showGoalMarker m = img [src <| markerImage m.c m.s, class "goal-marker"] []

showControls : Marker -> Int-> Html Msg
showControls gm c=
  div [ class "control-container"] [
        showGoalMarker gm,
        button  [onClick (Start),
                  classList [("btn", True), ("btn-sm", True)]] [
                    text "New game"
                ],
        button  [ onClick (NextMarker),
                  classList[("btn", True),("btn-sm", True)]] [
                    text "Next marker"
                ],
        button [  onClick (Restart),
                  classList[("btn", True),("btn-sm", True)]] [
                    text "Restart"
                ],
        div [] [
                  text <| String.concat ["Number of moves: ",
                                          (toString c)]
                ]
      ]
showWinScreen : Int -> Html Msg
showWinScreen c =
  div [ class "win"]  [
        div [ class "win-content",
              style [("grid-row","2")]] [
                div [] [
                          text <|
                          String.concat["You won in ", toString c, " moves!"]
                        ]
        ],
        div [ class "win-content",
              style [("grid-row","3")]] [
              button [onClick (Start),
                      classList[("btn", True),("btn-lg", True)]] [
                                  text "New game"
                      ],
              button [onClick (NextMarker),
                      classList[("btn", True),("btn-lg", True)]] [
                                  text "Next marker"
                      ],
              button [onClick (Restart),
                      classList[("btn", True),("btn-lg", True)]] [
                                  text "Restart"
                      ]
        ]
  ]

{- Should be reduntant.

{-| Styling for bottom layer -}
boardWrapper : Int -> Attribute msg
boardWrapper s = style (wrapper s)

{-| style for robot layer -}
robotWrapper : Int -> Attribute msg
robotWrapper s = style (("z-index","20") :: wrapper s)

{-| style for marker layer -}
markerWrapper : Int -> Attribute msg
markerWrapper s = style (("z-index","15") :: wrapper s)

{-| style for wall layer -}
wallWrapper : Int -> Attribute msg
wallWrapper s = style (("z-index", "10") :: wrapper s)

{-| basic layer stylings -}
wrapper : Int -> List (String, String)
wrapper s = 
  [("display", "grid"),
  ("grid-auto-columns", "5%"),
  ("grid-auto-rows", "5%"),
  ("position","absolute"),
  ("width", "auto"),
  ("height", "auto")
  ]
-}

allPos : Int -> List Pos
allPos size = allPosHelp size size

allPosHelp : Int -> Int -> List Pos
allPosHelp iter size = case iter of 
  i -> (zip (List.range 0 size) (List.repeat size i))::(allPosHelp (i-1) size)
  0 -> []

zip : List a -> List b -> List (a, b)
zip la lb = case (la, lb) of
  (a::ass, b::bs) -> (a, b)::(zip ass bs)
  (_, _) -> []
  ([], []) -> []
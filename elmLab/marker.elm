module Marker exposing (Marker, Symbol, symbols, markersGenerator)
{-| Markers
@docs Marker
@docs Symbol
@docs symbols
@docs markersGenerator
-}

import Common exposing (..)
import Random exposing (..)
{-| -}
type alias Marker = {c: Color, s: Symbol, i: Int}
{-| -}
type Symbol = Moon | Planet | Star | Gear
{-| -}
symbols : List Symbol
symbols = [Moon, Planet, Star, Gear]

markerCombinations : List ( Symbol, Color )
markerCombinations =  let
                        mColors = List.drop 1 colors
                      in
                        List.concat <| List.map2 (\s cs -> List.map (\c -> (s, c)) cs) symbols <| List.map (\x -> mColors) mColors
{-| -}
markersGenerator : Int -> Generator (List Marker)
markersGenerator i = flattenList <| List.map (markerGenerator i) markerCombinations

markerGenerator : Int -> (Symbol, Color) -> Generator Marker
markerGenerator i (s, c) = map (mkMarker c s) (int 1 i)

mkMarker : Color -> Symbol -> Int -> Marker
mkMarker c s i =   {c = c, s = s, i = i}

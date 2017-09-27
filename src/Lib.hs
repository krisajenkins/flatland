{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Lib
  ( render
  ) where

import           Control.Lens                 (Getting, Lens', makeLenses,
                                               preview, to, view, _Just)
import           Data.List                    (sortOn)
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid                  ((<>))
import           Data.Ord                     (Down (Down))
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Safe                         (headMay)
import           Text.PrettyPrint.ANSI.Leijen (Doc, blue, dullred, green,
                                               putDoc, red, string, white,
                                               yellow)

data Color
  = Red
  | White
  | Blue
  | Green
  deriving (Eq, Show, Ord)

data Point = Point
  { _px    :: Int
  , _py    :: Int
  , _pz    :: Int
  , _char  :: Char
  , _color :: Color
  } deriving (Eq, Show, Ord)

makeLenses ''Point

docColor :: Color -> Doc -> Doc
docColor White = white
docColor Red   = red
docColor Green = green
docColor Blue  = blue

type World = Set Point

aWorld :: World
aWorld =
  Set.fromList $
  frontDetails <> frontFace <> backFace <> (deepStrut 1 1) <> (deepStrut 1 8) <>
  chimney <>
  (deepStrut 8 1) <>
  (deepStrut 8 8) <>
  (deepStrut 4 11) <>
  (deepStrut 5 11)
  where
    backFace =
      [ Point 1 1 14 '#' Blue
      , Point 1 2 14 '#' Blue
      , Point 1 3 14 '#' Blue
      , Point 1 4 14 '#' Blue
      , Point 1 5 14 '#' Blue
      , Point 1 6 14 '#' Blue
      , Point 1 7 14 '#' Blue
      , Point 1 8 14 '#' Blue
      , Point 8 1 14 '#' Blue
      , Point 8 2 14 '#' Blue
      , Point 8 3 14 '#' Blue
      , Point 8 4 14 '#' Blue
      , Point 8 5 14 '#' Blue
      , Point 8 6 14 '#' Blue
      , Point 8 7 14 '#' Blue
      , Point 8 8 14 '#' Blue
      , Point 2 1 14 '#' Blue
      , Point 3 1 14 '#' Blue
      , Point 4 1 14 '#' Blue
      , Point 5 1 14 '#' Blue
      , Point 6 1 14 '#' Blue
      , Point 7 1 14 '#' Blue
      , Point 8 1 14 '#' Blue
      , Point 1 8 14 '#' Blue
      , Point 2 9 14 '#' Blue
      , Point 3 10 14 '#' Blue
      , Point 4 11 14 '#' Blue
      , Point 5 11 14 '#' Blue
      , Point 6 10 14 '#' Blue
      , Point 7 9 14 '#' Blue
      , Point 8 8 14 '#' Blue
      ]
    deepStrut x y =
      [ Point x y 2 '=' Blue
      , Point x y 3 '=' Blue
      , Point x y 4 '=' Blue
      , Point x y 5 '=' Blue
      , Point x y 6 '=' Blue
      , Point x y 7 '=' Blue
      , Point x y 8 '=' Blue
      , Point x y 9 '=' Blue
      , Point x y 10 '=' Blue
      , Point x y 11 '=' Blue
      , Point x y 12 '=' Blue
      , Point x y 13 '=' Blue
      ]
    chimney =
      [Point 2 9 6 '^' White, Point 2 10 6 '^' White, Point 2 11 6 '^' White]
    frontFace =
      [ Point 1 1 1 '#' Blue
      , Point 1 2 1 '#' Blue
      , Point 1 3 1 '#' Blue
      , Point 1 4 1 '#' Blue
      , Point 1 5 1 '#' Blue
      , Point 1 6 1 '#' Blue
      , Point 1 7 1 '#' Blue
      , Point 1 8 1 '#' Blue
      , Point 8 1 1 '#' Blue
      , Point 8 2 1 '#' Blue
      , Point 8 3 1 '#' Blue
      , Point 8 4 1 '#' Blue
      , Point 8 5 1 '#' Blue
      , Point 8 6 1 '#' Blue
      , Point 8 7 1 '#' Blue
      , Point 8 8 1 '#' Blue
      , Point 2 1 1 '#' Blue
      , Point 3 1 1 '#' Blue
      , Point 4 1 1 '#' Blue
      , Point 5 1 1 '#' Blue
      , Point 6 1 1 '#' Blue
      , Point 7 1 1 '#' Blue
      , Point 8 1 1 '#' Blue
      , Point 1 8 1 '#' Blue
      , Point 2 9 1 '#' Blue
      , Point 3 10 1 '#' Blue
      , Point 4 11 1 '#' Blue
      , Point 5 11 1 '#' Blue
      , Point 6 10 1 '#' Blue
      , Point 7 9 1 '#' Blue
      , Point 8 8 1 '#' Blue
      ]
    frontDetails =
      [ Point 3 7 1 '+' Red
      , Point 6 7 1 '+' Red
      , Point 4 1 0 '|' Green
      , Point 5 1 0 '|' Green
      , Point 4 2 0 '|' Green
      , Point 5 2 0 '|' Green
      , Point 4 3 0 '_' Green
      , Point 5 3 0 '_' Green
      , Point 4 1 (-1) '|' Green
      , Point 5 1 (-1) '|' Green
      , Point 4 2 (-1) '|' Green
      , Point 5 2 (-1) '|' Green
      , Point 4 3 (-1) '_' Green
      , Point 5 3 (-1) '_' Green
      ]

renderPoint
  :: (Ord a, Eq a)
  => World
  -> Getting a Point a
  -> (Getting a Point a, Getting a Point a)
  -> (a, a)
  -> Doc
renderPoint world sortLens (axis1Lens, axis2Lens) (axis1Value, axis2Value) =
  case mPoint of
    Nothing -> string " "
    Just point -> docColor (view color point) $ string $ pure $ view char point
  where
    mPoint =
      headMay $
      sortOn (view sortLens) $
      filter
        (\point ->
           (view axis1Lens point == axis1Value) &&
           (view axis2Lens point == axis2Value)) $
      Set.toList world

renderView title sortLens (axis1Lens, axis2Lens) (axis1Range, axis2Range) = do
  putDoc $ yellow $ string title
  putStrLn ""
  mapM_ printRow axis2Range
  where
    printRow y = do
      let docs :: [Doc] =
            (\x -> renderPoint aWorld sortLens (axis1Lens, axis2Lens) (x, y)) <$>
            axis1Range
      putDoc (mconcat docs)
      putStrLn ""

render = do
  renderView
    "Plan"
    (py . to negate)
    (px, pz)
    ([(-2) .. 15], reverse [(-2) .. 15])
  putStrLn "-----"
  renderView "Elevation" pz (px, py) ([(-2) .. 15], reverse [(-2) .. 15])
  putStrLn "-----"
  renderView "Section" px (pz, py) (reverse [(-2) .. 15], reverse [(-2) .. 15])

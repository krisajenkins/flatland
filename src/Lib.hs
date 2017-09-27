{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Lib
  ( render
  ) where

import           Control.Lens                 (Getting, Lens', makeLenses,
                                               preview, view, _Just)
import           Data.List                    (sortOn)
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid                  ((<>))
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Safe                         (headMay)
import           Text.PrettyPrint.ANSI.Leijen (Doc, blue, dullred, green,
                                               putDoc, red, string, white,
                                               yellow)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

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

type World = Set Point

aWorld :: World
aWorld = Set.fromList (w <> l <> h)
  where
    w =
      [ Point 1 5 2 'W' Red
      , Point 1 5 8 'W' Red
      , Point 1 4 2 'W' Red
      , Point 1 4 8 'W' Red
      , Point 1 3 2 'W' Red
      , Point 1 3 8 'W' Red
      , Point 1 3 5 'W' Red
      , Point 1 2 2 'W' Red
      , Point 1 2 8 'W' Red
      , Point 1 2 5 'W' Red
      , Point 1 1 3 'W' Red
      , Point 1 1 4 'W' Red
      , Point 1 1 6 'W' Red
      , Point 1 1 7 'W' Red
      ]
    l =
      [ Point 2 5 1 'L' Blue
      , Point 2 4 1 'L' Blue
      , Point 2 3 1 'L' Blue
      , Point 2 2 1 'L' Blue
      , Point 2 1 1 'L' Blue
      , Point 3 1 1 'L' Blue
      , Point 4 1 1 'L' Blue
      , Point 5 1 1 'L' Blue
      , Point 6 1 1 'L' Blue
      ]
    h =
      [ Point 2 1 8 'H' Green
      , Point 6 1 8 'H' Green
      , Point 2 1 7 'H' Green
      , Point 6 1 7 'H' Green
      , Point 2 1 6 'H' Green
      , Point 6 1 6 'H' Green
      , Point 2 1 5 'H' Green
      , Point 3 1 5 'H' Green
      , Point 4 1 5 'H' Green
      , Point 5 1 5 'H' Green
      , Point 6 1 5 'H' Green
      , Point 2 1 4 'H' Green
      , Point 6 1 4 'H' Green
      , Point 2 1 3 'H' Green
      , Point 6 1 3 'H' Green
      , Point 2 1 2 'H' Green
      , Point 6 1 2 'H' Green
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

docColor :: Color -> Doc -> Doc
docColor White = white
docColor Red   = red
docColor Green = green
docColor Blue  = blue

renderView title sortLens (axis1Lens, axis2Lens) (axis1Range, axis2Range) = do
  putDoc $ yellow $ string title
  putStrLn ""
  mapM_ printRow axis2Range
  where
    printRow y = do
      let docs :: [Doc] =
            fmap
              (\x -> renderPoint aWorld sortLens (axis1Lens, axis2Lens) (x, y))
              axis1Range
      putDoc ((mconcat docs))
      putStrLn ""

render = do
  renderView "Plan" py (px, pz) ([0 .. 10], reverse [0 .. 10])
  putStrLn "-----"
  renderView "Elevation" pz (px, py) ([0 .. 10], reverse [0 .. 10])
  putStrLn "-----"
  renderView "Section" px (pz, py) (reverse [0 .. 10], reverse [0 .. 10])

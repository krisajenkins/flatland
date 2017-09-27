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
                                               putDoc, red, string, white)

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
aWorld =
  Set.fromList
    [ Point 1 1 1 '.' White
    , Point 2 1 1 'x' Red
    , Point 3 1 1 'x' Red
    , Point 4 1 1 'x' Red
    , Point 5 1 1 'x' Red
    , Point 1 2 1 'y' Blue
    , Point 1 3 1 'y' Blue
    , Point 1 4 1 'y' Blue
    , Point 1 1 2 'z' Green
    , Point 1 1 3 'z' Green
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
  putStrLn title
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

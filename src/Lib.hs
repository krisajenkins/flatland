{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Lib
  ( someFunc
  ) where

import           Control.Lens (Getting, Lens', makeLenses, preview, view, _Just)
import           Data.List    (sortOn)
import           Data.Maybe   (fromMaybe)
import           Data.Set     (Set)
import qualified Data.Set     as Set
import           Safe         (headMay)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Point = Point
  { _px   :: Int
  , _py   :: Int
  , _pz   :: Int
  , _char :: Char
  } deriving (Eq, Show, Ord)

makeLenses ''Point

type World = Set Point

aWorld :: World
aWorld =
  Set.fromList
    [ Point 1 1 1 '.'
    , Point 2 1 1 'x'
    , Point 3 1 1 'x'
    , Point 4 1 1 'x'
    , Point 5 1 1 'x'
    , Point 1 2 1 'y'
    , Point 1 3 1 'y'
    , Point 1 4 1 'y'
    , Point 1 1 2 'z'
    , Point 1 1 3 'z'
    ]

elevationChar
  :: (Ord a, Eq a)
  => World
  -> Getting a Point a
  -> (Getting a Point a, Getting a Point a)
  -> (a, a)
  -> Char
elevationChar world sortLens (axis1Lens, axis2Lens) (axis1Value, axis2Value) = do
  fromMaybe ' ' $
    preview (_Just . char) $
    headMay $
    sortOn (view sortLens) $
    filter
      (\point ->
         (view axis1Lens point == axis1Value) &&
         (view axis2Lens point == axis2Value)) $
    Set.toList world

renderView title sortLens (axis1Lens, axis2Lens) (axis1Range, axis2Range) = do
  putStrLn title
  mapM_ printRow axis2Range
  where
    printRow y =
      putStrLn $
      fmap
        (\x -> elevationChar aWorld sortLens (axis1Lens, axis2Lens) (x, y))
        axis1Range

main = do
  renderView "Plan" py (px, pz) ([0 .. 10], reverse [0 .. 10])
  putStrLn "-----"
  renderView "Elevation" pz (px, py) ([0 .. 10], reverse [0 .. 10])
  putStrLn "-----"
  renderView "Section" px (pz, py) (reverse [0 .. 10], reverse [0 .. 10])

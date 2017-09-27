{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Lib
  ( someFunc
  ) where

import           Control.Lens (makeLenses, preview, _Just)
import           Data.List    (sortOn)
import           Data.Maybe   (fromMaybe)
import           Data.Set     (Set)
import qualified Data.Set     as Set
import           Safe         (headMay)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Point = Point
  { _x    :: Int
  , _y    :: Int
  , _z    :: Int
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

elevationChar :: World -> (Int, Int) -> Char
elevationChar world (x, y) = do
  fromMaybe ' ' $
    preview (_Just . char) $
    headMay $
    sortOn _z $
    filter (\(Point a b c l) -> (a == x) && (b == y)) $ Set.toList world

elevation :: IO ()
elevation = do
  putStrLn "Elevation"
  let yS = reverse $ [0 .. 10]
  mapM_ printRow yS
  where
    printRow y = do
      let xS = [0 .. 10]
          chars = fmap (\x -> elevationChar aWorld (x, y)) xS
      putStrLn chars

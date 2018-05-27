{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import qualified Data.BTree as BTree

import Data.Text.Lazy (pack)
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Web.Scotty

defaultWidth = 512
defaultHeight = 512

renderOptions w h = SVGOptions { _size = dims2D w h
                            , _generateDoctype = True
                            , _svgAttributes = []
                            , _svgDefinitions = Nothing
                            , _idPrefix = ""
                            }  
  
main = scotty 9292 $ do
  get "/" $ do
    html $ render defaultWidth defaultHeight

  get "/:width/:height" $ do
    width <- param "width"
    height <- param "height"
    html $ render width height

  where render w h = pack $ show $
          renderDia SVG (renderOptions w h) $ BTree.diagram

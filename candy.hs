{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}

-- not sure if I like "rec" or "do"
-- http://stackoverflow.com/questions/5405850/how-does-the-haskell-rec-keyword-work
-- http://stackoverflow.com/questions/16726659/should-do-notation-be-avoided-in-haskell

import Reflex.Dom
import Data.Map
import Data.Text (Text, pack)
import System.Random
import Control.Monad.Fix

-------------------------------------------------------------------------------

ns :: Maybe Text
ns = Just "http://www.w3.org/2000/svg"

svgAttributes :: Map Text Text
svgAttributes = ( "style" =: "width:300px; height:150px; background-color:#F0F0F0;" )

colors :: [ Text ]
colors = fmap pack [ "#DEB225", "#EB5044", "#602FFF", "#D5F531" ]

squareAttributes :: (Int, Int) -> Text -> Map Text Text
squareAttributes ( a, b ) color = fromList [("x", ( pack . show ) a ), ("y", ( pack . show ) b ), ("width", "10"), ("height", "10"), ("fill", color )]

svgButton :: DomBuilder t m => Maybe ( Map Text Text ) -> m a -> m (Event t ())
svgButton attr child = case attr of
  Just attributes -> do
    (e, _) <- element "svg" ( def & initialAttributes .~ mapKeys (AttributeName Nothing) attributes & namespace .~ Just "http://www.w3.org/2000/svg" ) child
    return $ domEvent Click e
  Nothing         -> do
    (e, _) <- element "svg" ( def & namespace .~ Just "http://www.w3.org/2000/svg" ) child
    return $ domEvent Click e

square :: DomBuilder t m => Text -> m a -> m (Event t ())
square color child = do
  (e, _) <- element "rect" ( def & initialAttributes .~ mapKeys (AttributeName Nothing) ( squareAttributes ( 0 , 0 ) color ) & namespace .~ Just "http://www.w3.org/2000/svg" ) child
  return $ domEvent Click e

-- later, an important discussion on these type constraints (with pictures)
piece :: ( DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m )  => m ( Event t () )
piece = do
  rec (e, _) <- el' "div" $ display =<< count (domEvent Click e)
  return $ domEvent Click e


tile :: ( DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m ) => ( Int , Int ) -> m ( Event t () )
tile x = do
  rec let attrs = fmap (\n -> squareAttributes x $ colors !! ( mod n 2 )) timesPressed
      (e, _) <- elDynAttrNS' ( Just "http://www.w3.org/2000/svg" ) "rect" attrs blank
      timesPressed <- count $ domEvent Click e
  return $ domEvent Click e

t :: RandomGen g => g -> (Int, g)
t = randomR (0, 3 :: Int)

n :: Int
n = fst <$> randomR (0, 3 :: Int) $ mkStdGen 42

-- TUTORIAL
--
-- > :t sequence
-- sequence  :: (Traversable t, Monad m) => t (m a) -> m (t a)
-- > :t sequence_
-- sequence_ :: (Foldable t   , Monad m) => t (m a) -> m ()
-- > :t mapM
-- mapM  :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
-- > :t mapM_
-- mapM_ :: (Foldable t   , Monad m) => (a -> m b) -> t a -> m ()
--
-- Prelude Reflex.Dom> :t map
-- map :: (a -> b) -> [a] -> [b]
-- Prelude Reflex.Dom> :t fmap
-- fmap :: Functor f => (a -> b) -> f a -> f b


main = mainWidget $ do

  el "div" $ do
    -- in this monad each computation returns an HTML artifact
    btn <- ( svgButton ( Just svgAttributes ) ) $ do
      ( square $ colors !! n  ) blank >>  sequence_  ( fmap tile $ [ (20 ,  0), (20,  20), (20,  40) ] )
    c   <- count ( btn )
    el "div" $ dynText $ fmap ( pack . show ) $ c
    -- help from IRC
    -- rec (e, _) <- elAttr' "div" ( fromList [("style", "background-color:#DEB225; width:12pt;")])  $ display =<< count (domEvent Click e)
    piece
    return ()
  return ()

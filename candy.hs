{-# LANGUAGE OverloadedStrings #-}

import Reflex.Dom
import Data.Map
import Data.Text (Text, pack)

-------------------------------------------------------------------------------

ns :: Maybe Text
ns = Just "http://www.w3.org/2000/svg"

svgAttributes :: Map Text Text
svgAttributes = ( "style" =: "width:300px; height:150px; background-color:#F0F0F0;" )

squareAttributes :: Map Text Text
squareAttributes = fromList [("x", "0"), ("y", "0"), ("width", "10"), ("height", "10"), ("fill", "#F0F0F0")]

svgButton :: DomBuilder t m => Maybe ( Map Text Text ) -> m a -> m (Event t ())
svgButton attr child = case attr of
  Just attributes -> do
    (e, _) <- element "svg" ( def & initialAttributes .~ mapKeys (AttributeName Nothing) attributes & namespace .~ Just "http://www.w3.org/2000/svg" ) child
    return $ domEvent Click e
  Nothing         -> do
    (e, _) <- element "svg" ( def & namespace .~ Just "http://www.w3.org/2000/svg" ) child
    return $ domEvent Click e


main = mainWidget $ do

  el "div" $ do
    -- in this monad each computation returns an HTML artifact
    btn <- svgButton ( Just svgAttributes ) $ blank
    c   <- count ( btn )
    el "div" $ dynText $ fmap ( pack . show ) $ c
    return ()
  return ()

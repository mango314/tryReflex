{-# LANGUAGE OverloadedStrings #-}

import Reflex.Dom
import Data.Map
import Data.Text (Text, pack)

-- SYMBOL TABLE
-- (&) :: a -> (a -> b) -> b
-- (=:) :: k -> a -> Map k a
-- ($) :: (a -> b) -> a -> b

div :: MonadWidget t m => m a -> m a
div = el "div"

css :: Map Text Text
css = ( "style" =: "color:#EB723C; font-family: Helvetica;" )

title :: MonadWidget t m => Text -> m ()
title x = elAttr "div" css $ el "h1" $ text x

cssP :: Map Text Text
cssP = ( "style" =: "color:black; font-family: Helvetica; font-size:20px;"  )

p :: MonadWidget t m => Text -> m ()
p x = elAttr "p" cssP $ text x

cssTxt :: Map Text Text
cssTxt = ( "style" =: "font-family: Courier;")

-- (&)  :: a -> (a -> b) -> b
-- (.~) :: ASetter s t a b -> b -> s -> t

-- where do you put a style?
svg :: MonadWidget t m => m ()
svg =  do
  element "svg" ( def & namespace .~ Just "http://www.w3.org/2000/svg") blank
  return ()

svg' :: MonadWidget t m => m ()
svg' = do
  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" def blank
  return ()

cssSvg :: Map Text Text
cssSvg = ( "style" =: "width:300px; height:150px; background-color:#F0F0F0; margin-top:5px;" )

svgAttr :: MonadWidget t m => Map Text Text -> m ()
svgAttr attrs =  do
  element "svg" ( def & namespace .~ Just "http://www.w3.org/2000/svg" & initialAttributes .~ mapKeys (AttributeName Nothing) attrs ) blank
  return ()

--svgDyn :: MonadWidget t m => Dynamic t (Map Text Text) -> m a -> m a
--svgDyn attrs = do
--  snd <$> element "svg" ( def & namespace .~ Just "http://www.w3.org/2000/svg" & initialAttributes .~ mapKeys (AttributeName Nothing) attrs ) blank
--  return ()




main = mainWidget $ do
  title "Create your own bar-chart!"
  p "type a list of numbers"
  btn <- el "div" $ button "xyz"
--dynText $ fmap (pack . show)  $    btn & count
--el "div" $ ( dynText . fmap (pack . show) ) =<< ( btn >>= count )
--dynText . fmap (pack . show) =<<
--el "div" $ display =<< count =<< button "Click Me!"
  el "div" $ display =<< count =<< btn
  el "div" $ ( dynText . fmap (pack . show) ) =<< count =<< button "Click Me!"
  svgAttr cssSvg

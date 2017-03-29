{-# LANGUAGE OverloadedStrings #-}

import Reflex.Dom
import Data.Map
import Data.Text (Text, pack)

-- SYMBOL TABLE
-- (&) :: a -> (a -> b) -> b
-- (=:) :: k -> a -> Map k a
-- ($) :: (a -> b) -> a -> b

-- A DILEMMA
-- this code has two audiences
-- 1) end users (who do not see code; only finished product = compiled html)
-- 2) other programmers (who do not seee end product, just code)
-- and this code should be readable to both groups of coders

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
svg :: MonadWidget t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
svg =  element "svg" ( def & namespace .~ Just "http://www.w3.org/2000/svg")

svg' :: MonadWidget t m => m ()
svg' = do
  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" def blank
  return ()

cssSvg :: Map Text Text
cssSvg = ( "style" =: "width:300px; height:150px; background-color:#F0F0F0; margin-top:5px;" )

svgAttr :: MonadWidget t m => Map Text Text -> m a -> m a
svgAttr attrs child =  snd <$> element "svg" ( def & namespace .~ Just "http://www.w3.org/2000/svg" & initialAttributes .~ mapKeys (AttributeName Nothing) attrs ) child

-- shift towards combining all the various element or functions w/ or w/o attributes, dynamic or not
circle :: MonadWidget t m => Map Text Text -> m a -> m a
circle attrs child =  snd <$> element "circle" ( def & namespace .~ Just "http://www.w3.org/2000/svg" & initialAttributes .~ mapKeys (AttributeName Nothing) attrs ) child


--svgDyn :: MonadWidget t m => Dynamic t (Map Text Text) -> m a -> m a
--svgDyn attrs = do
--  snd <$> element "svg" ( def & namespace .~ Just "http://www.w3.org/2000/svg" & initialAttributes .~ mapKeys (AttributeName Nothing) attrs ) blank
--  return ()

display' :: (PostBuild t m, DomBuilder t m, Show a) => Dynamic t a -> m ()
display' = dynText . fmap (pack . show)

button' :: DomBuilder t m => Text -> Maybe ( Map Text Text ) -> m (Event t ())
button' t attr = case attr of
  Just attributes -> do
    (e, _) <- element "button" ( def & initialAttributes .~ mapKeys (AttributeName Nothing) attributes ) $ text t
    return $ domEvent Click e
  Nothing         -> do
    (e, _) <- element "button" def $ text t
    return $ domEvent Click e

cssButton :: Map Text Text
cssButton = ("style" =: "width:75px;font-weight:bold;" )

cssCircle :: Int -> Map Text Text
cssCircle n = fromList [ ("cx", ( (pack. show) ( 25*(1 + (mod n 11) ) ) )), ("cy", "50"), ("r", "10"), ("fill", "#A0A0A0"), ("stroke", "none") ]

-- http://stackoverflow.com/questions/38268962/a-single-svg-element-with-reflex-frp
ns :: Maybe Text
ns = Just "http://www.w3.org/2000/svg"

main = mainWidget $ do
  title "Click button Circle Hops around the Screen"
  el "del" $ p "type a list of numbers"

  el "div" $ do
    btn <- button' "Click Me!" $ Just cssButton
    elAttr "div" ("style" =: "color:#A0A0A0; width:195px; display:inline-block;padding-left:5px;" ) $ text "# of clicks:"
    count (btn) >>= ( elAttr "div" ("style" =: "font-family:Helvetica; width:25px; display: inline-block;text-align:right;"). dynText  . fmap (pack . show . (\x -> x + 1) ) )
    --(\ x -> el "div" $ svgAttr cssSvg $ ( circle . cssCircle ) x  blank ) 5
    c   <- count (btn)
    let
      circ = (\x -> elDynAttrNS' (Just "http://www.w3.org/2000/svg") "circle" ( fmap cssCircle x ) blank ) ( c )
      in el "div" $ elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" ( constDyn ( "style" =: "width:300px; height:150px; background-color:#F0F0F0;" ) ) circ
  return ()

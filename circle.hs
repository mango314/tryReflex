import Reflex.Dom
import qualified Data.Map as Map

main = mainWidget $ myDiv

myDiv = do
  let attrs = constDyn $ Map.fromList
               [ ("width" , "500")
               , ("height" , "250")
               ]
  let cAttrs = constDyn $ Map.fromList
               [ ("cx", "50")
               , ("cy", "50")
               , ("r", "40")
               , ("stroke", "green")
               , ("stroke-width", "3")
               , ("fill",  "yellow" )
               ]

  s <- elSvg "svg" attrs (elSvg "circle" cAttrs (return ()))
  return ()

elSvg tag a1 a2 = do
  elDynAttrNS' ns tag a1 a2
  return ()

ns :: Maybe String
ns = Just "http://www.w3.org/2000/svg"

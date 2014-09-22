{-# LANGUAGE ViewPatterns #-}

import Data.Sequence hiding (filter, sort)
import qualified Data.Foldable as F
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Data.List
import Data.Maybe
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import qualified Data.GraphViz.Types.Generalised as G
import Text.XML.HXT.Core
import Text.Printf
import System.Environment

parseDot :: Maybe FilePath -> IO (G.DotGraph String)
parseDot mbPath = do
  text <- case mbPath of
            Just path -> TIO.readFile path
            Nothing   -> TIO.getContents
  return (parseDotGraph text)

graphml :: String
graphml = "http://graphml.graphdrawing.org/xmlns/graphml" 

yed :: String
yed = "http://www.yworks.com/xml/graphml" 

graphXml :: ArrowXml a => G.DotGraph String -> a XmlTree XmlTree
graphXml dot = 
  let g = mkQName "g" "graphml" graphml
      sts = G.graphStatements dot
      fullGraph = undeclaredNodes sts >< sts
      defcolor = fromMaybe "#FFFFCC" $ getColor $ concat [ga | G.GA (GraphAttrs ga) <- F.toList sts]
  in mkqelem g [sattr "xmlns" graphml, sattr "xmlns:y" yed,
                sattr "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance",
                sattr "xsi:schemaLocation" "http://graphml.graphdrawing.org/xmlns/graphml http://www.yworks.com/xml/schema/graphml/1.0/ygraphml.xsd"] $
       [mkelem "key" [sattr "for" "node",
                      sattr "id" "d0",
                      sattr "yfiles.type" "nodegraphics"] [],
        mkelem "key" [sattr "for" "node",
                      sattr "id" "d1",
                      sattr "name" "description",
                      sattr "attr.type" "string"] [],
        mkelem "key" [sattr "for" "edge",
                      sattr "id" "d2",
                      sattr "yfiles.type" "nodegraphics"] [],
        mkelem "key" [sattr "for" "edge",
                      sattr "id" "d3",
                      sattr "name" "description",
                      sattr "attr.type" "string"] [],
        mkelem "key" [sattr "for" "graphml",
                      sattr "id" "d4",
                      sattr "yfiles.type" "resources"] [],
        mkelem "key" [sattr "for" "node",
                      sattr "id" "d6",
                      sattr "yfiles.type" "nodegraphics"] [],
        mkelem "graph" ([sattr "edgedefault" "directed",
                         sattr "parse.order" "free",
                         sattr "parse.edges" (show $ nEdges fullGraph),
                         sattr "parse.nodes" (show $ nNodes fullGraph)] ++ idAttr (G.graphID dot)) (run defcolor fullGraph),
        mkelem "data" [sattr "key" "d4"] [
          mkqelem (mkQName "y" "Resources" "") [] [] ]
        ]

nNodes sts = sum $ seqmap go sts
  where
    go (G.DN _) = 1
    go (G.SG (G.DotSG _ _ subgraph)) = nNodes subgraph
    go _ = 0

nEdges sts = sum $ seqmap go sts
  where
    go (G.DE _) = 1
    go (G.SG (G.DotSG _ _ subgraph)) = nEdges subgraph
    go _ = 0

seqmap f xs = F.toList $ fmap f xs

showGID :: Maybe GraphID -> String
showGID Nothing = ""
showGID (Just (Str str)) = T.unpack str
showGID (Just (Num (Int int))) = "i" ++ show int
showGID (Just (Num (Dbl dbl))) = "d" ++ show dbl

idAttr Nothing = []
idAttr x@(Just _) = [sattr "id" (showGID x)]

idAttr' s Nothing = [sattr "id" s]
idAttr' s x = [sattr "id" (showGID x ++ s)]

allNodeNames :: G.DotStatements String -> [String]
allNodeNames sts = concatMap go (F.toList sts)
  where
    go (G.DN (G.DotNode nid _)) = [nid]
    go (G.SG (G.DotSG _ _ sg))  = concatMap go (F.toList sg)
    go _ = []

undeclaredNodes :: G.DotStatements String -> G.DotStatements String
undeclaredNodes sts =
  let allUsed = nub $ sort $ concat [ [from, to] | G.DE (G.DotEdge from to _) <- F.toList sts ]
      declaredNodes = allNodeNames sts
      mkNode name = G.DotNode name [Label (StrLabel $ T.pack name)]
  in  fromList $ map (G.DN . mkNode) $ filter (`notElem` declaredNodes) allUsed

run :: ArrowXml a => String -> G.DotStatements String -> [a XmlTree XmlTree]
run baseClr sts = concat $ seqmap fromRoot sts
  where
    defcolor = fromMaybe baseClr $ getColor $ concat $
                [ga | G.GA (G.GraphAttrs ga) <- concatMap F.toList [sg | G.SG (G.DotSG _ _ sg) <- F.toList sts]]

    fromRoot (G.SG (G.DotSG cl gid subgraph)) = seqmap go subgraph
    fromRoot (G.DE (G.DotEdge from to _)) = [mkelem "edge" [sattr "source" from,
                                                            sattr "target" to,
                                                            sattr "id" (from ++ to) ] [] ]
    fromRoot (G.DN (G.DotNode nid attrs)) = [mkelem "node" [sattr "id" nid] [ynode (getLabel attrs) (Just defcolor)] ]
    fromRoot x = [cmt (show x)]

    go :: ArrowXml a => G.DotStatement String -> a XmlTree XmlTree
    go (G.SG (G.DotSG _ gid subgraph)) =
      mkelem "node" (idAttr gid ++ [sattr "yfiles.foldertype" "group"]) [
        ygroup (getLabel $ graphAttrs subgraph) (getColor $ graphAttrs subgraph),
        mkelem "graph" (idAttr' ":g" gid) (seqmap go subgraph) ]
    go (G.DN (G.DotNode nid attrs)) =
        mkelem "node" [sattr "id" nid] [ynode (getLabel attrs) (getColor attrs),
                                        mkelem "data" [sattr "key" "d1"] [] ]
    go (G.DE (G.DotEdge from to _)) =
        mkelem "edge" [sattr "source" from,
                       sattr "target" to,
                       sattr "id" (from ++ to)] [mkelem "data" [sattr "key" "d3"] [] ]
    go (G.GA ga) = cmt (show ga)

    graphAttrs sts = case [ga | G.GA (G.GraphAttrs ga) <- F.toList sts] of
                       [] -> []
                       (ga:_) -> ga

    shapeNode       = mkQName "y" "ShapeNode" ""
    nodeLabel       = mkQName "y" "NodeLabel" ""
    geometry        = mkQName "y" "Geometry" ""
    fill            = mkQName "y" "Fill" ""
    border          = mkQName "y" "BotderStyle" ""
    shape           = mkQName "y" "Shape" ""
    proxyAutoBounds = mkQName "y" "ProxyAutoBoundsNode" ""
    realizers       = mkQName "y" "Realizers" ""
    groupNode       = mkQName "y" "GroupNode" ""
    state           = mkQName "y" "State" ""

    clrAttr Nothing = sattr "color" defcolor
    clrAttr (Just clr) = sattr "color" clr

    ynode label color =
      mkelem "data" [sattr "key" "d0"] [
        mkqelem shapeNode [] [
          mkqelem geometry [sattr "height" "30.0", sattr "width" "30.0", sattr "x" "0.0", sattr "y" "0.0"] [],
          mkqelem fill [clrAttr color, sattr "transparent" "false"] [],
          mkqelem border [sattr "color" "#000000", sattr "type" "line", sattr "width" "1.0"] [],
          mkqelem shape [sattr "type" "ellipse"] [],
          mkqelem nodeLabel [sattr "alignment" "center",
                             sattr "autoSizePolicy" "content",
                             sattr "hasBackgroundColor" "false",
                             sattr "modelName" "internal",
                             sattr "modelPosition" "c" ] [txt label] ] ]
    
    ygroup label color =
      let lbl = mkqelem nodeLabel [sattr "height" "25.0",
                                sattr "alignment" "right",
                                sattr "autoSizePolicy" "node_width",
                                sattr "borderDistance" "0.0",
                                sattr "modelName" "internal",
                                sattr "modelPosition" "t",
                                sattr "visible" "true"] [txt label]
      in  mkelem "data" [sattr "key" "d6"] [
            mkqelem proxyAutoBounds [] [
              mkqelem realizers [sattr "active" "0"] [
               mkqelem groupNode [] [
                 mkqelem fill [clrAttr color, sattr "transparent" "false"] [],
                 mkqelem state [sattr "closed" "false",
                                sattr "closedHeight" "50.0",
                                sattr "closedWidth"  "50.0",
                                sattr "innerGraphDisplayEnabled" "false"] [],
                 mkqelem shape [sattr "type" "ellipse"] [],
                 lbl
               ],
               mkqelem groupNode [] [
                 mkqelem fill [clrAttr color, sattr "transparent" "false"] [],
                 mkqelem state [sattr "closed" "true",
                                sattr "closedHeight" "50.0",
                                sattr "closedWidth"  "50.0",
                                sattr "innerGraphDisplayEnabled" "false"] [],
                 lbl
                ]
              ]
            ]
          ]

getLabel attrs =
    case [lbl | Label lbl <- attrs] of
      [] -> ""
      (StrLabel l:_) -> renderEscapes $ T.unpack l
      (l:_) -> show l

renderEscapes :: String -> String
renderEscapes str = go str
  where
    go [] = []
    go [c] = [c]
    go ('\\':'n':cs) = '\n': go cs
    go ('\\':'l':cs) = '\n': go cs
    go ('\\':'r':cs) = '\n': go cs
    go (c:cs) = c: go cs

getColor attrs =
    case concat [clr | Color clr <- attrs] of
       [] -> Nothing
       (WC (RGB r g b) _:_) -> Just $  printf "#%02x%02x%02x" r g b
       _ -> Nothing

main :: IO ()
main = do
  argv <- getArgs
  (mbInp,mbOutp) <- case argv of
                      [] -> return (Nothing, Nothing)
                      [p] -> return (Just p, Nothing)
                      [p,q] -> return (Just p, Just q)
                      _ -> do
                           printf "Synopsis: dot2graphml [INPUT.dot] [OUTPUT.graphml]"
                           fail "Invalid command line."
  graph <- parseDot mbInp
  runX $ root [] [graphXml graph >>> uniqueNamespacesFromDeclAndQNames] >>> propagateNamespaces >>> writeDocument [withCheckNamespaces yes, withIndent yes] (fromMaybe "-" mbOutp)
  return ()


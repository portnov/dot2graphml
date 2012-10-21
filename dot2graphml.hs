{-# LANGUAGE ViewPatterns #-}

import Data.Sequence
import qualified Data.Foldable as F
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Data.List
import Data.Maybe
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import qualified Data.GraphViz.Types.Generalised as G
import Text.XML.HXT.Core
import System.Environment

parseDot :: FilePath -> IO (G.DotGraph String)
parseDot path = do
  text <- TIO.readFile path
  return (parseDotGraph text)

graphml :: String
graphml = "http://graphml.graphdrawing.org/xmlns/graphml" 

yed :: String
yed = "http://www.yworks.com/xml/graphml" 

graphXml :: ArrowXml a => G.DotGraph String -> a XmlTree XmlTree
graphXml dot = 
  let key = mkQName "" "key" graphml
      graph = mkName "graph"
      node  = mkQName "" "node" graphml
      g = mkQName "g" "graphml" graphml
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
                         sattr "parse.edges" (show $ nEdges $ G.graphStatements dot),
                         sattr "parse.nodes" (show $ nNodes $ G.graphStatements dot)] ++
                         idAttr (G.graphID dot)) (run $ G.graphStatements dot),
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

showGID Nothing = ""
showGID (Just (Str str)) = T.unpack str
showGID (Just (Int int)) = "i" ++ show int
showGID (Just (Dbl dbl)) = "d" ++ show dbl

findNode :: String -> G.DotStatements String -> String
findNode name sts =
    case fromJust $ listToMaybe $ seqmap (go []) sts of
      [] -> name
      [x] -> x
      (_:xs) -> intercalate "::" xs
  where
    go :: [String] -> G.DotStatement String -> [String]
    go acc x
      | G.SG (G.DotSG _ gid subgraph) <- x = concat $ seqmap (go (acc ++ [showGID gid])) subgraph
      | G.DN (G.DotNode nid _) <- x, nid == name = acc ++ [name]
      | otherwise = []

idAttr Nothing = []
idAttr x@(Just _) = [sattr "id" (showGID x)]

idAttr' s Nothing = [sattr "id" s]
idAttr' s x = [sattr "id" (showGID x ++ s)]

run :: ArrowXml a => G.DotStatements String -> [a XmlTree XmlTree]
run sts = concat $ seqmap fromRoot sts
  where
    fromRoot (G.SG (G.DotSG cl gid subgraph)) = seqmap go subgraph
    fromRoot (G.DE (G.DotEdge from to _)) = [mkelem "edge" [sattr "source" from,
                                                            sattr "target" to,
                                                            sattr "id" (from ++ to) ] [] ]
    fromRoot (G.DN (G.DotNode nid attrs)) = [mkelem "node" [sattr "id" nid] [ynode $ getLabel attrs] ]
    fromRoot x = [cmt (show x)]

    go :: ArrowXml a => G.DotStatement String -> a XmlTree XmlTree
    go (G.SG (G.DotSG _ gid subgraph)) =
      mkelem "node" (idAttr gid ++ [sattr "yfiles.foldertype" "group"]) [
        ygroup (getLabel $ graphAttrs subgraph),
        mkelem "graph" (idAttr' ":g" gid) (seqmap go subgraph) ]
    go (G.DN (G.DotNode nid attrs)) =
        mkelem "node" [sattr "id" nid] [ynode $ getLabel attrs,
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

    ynode label =
      mkelem "data" [sattr "key" "d0"] [
        mkqelem shapeNode [] [
          mkqelem geometry [sattr "height" "30.0", sattr "width" "30.0", sattr "x" "0.0", sattr "y" "0.0"] [],
          mkqelem fill [sattr "color" "#CCCCFF", sattr "transparent" "false"] [],
          mkqelem border [sattr "color" "#000000", sattr "type" "line", sattr "width" "1.0"] [],
          mkqelem shape [sattr "type" "rectangle"] [],
          mkqelem nodeLabel [sattr "alignment" "center",
                             sattr "autoSizePolicy" "content",
                             sattr "fontFamily" "Dialog",
                             sattr "fontSize" "12",
                             sattr "fontStyle" "plain",
                             sattr "hasBackgroundColor" "falsee",
                             sattr "hasLineColor" "false",
                             sattr "modelName" "internal",
                             sattr "modelPosition" "c",
                             sattr "textColor" "#000000" ] [txt label] ] ]
    
    ygroup label =
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
                 mkqelem state [sattr "closed" "false",
                                sattr "closedHeight" "50.0",
                                sattr "closedWidth"  "50.0",
                                sattr "innerGraphDisplayEnabled" "false"] [],
                 mkqelem shape [sattr "type" "rectangle"] [],
                 lbl
               ],
               mkqelem groupNode [] [
                 mkqelem state [sattr "closed" "true",
                                sattr "closedHeight" "50.0",
                                sattr "closedWidth"  "50.0",
                                sattr "innerGraphDisplayEnabled" "false"] [],
                 lbl
                ]
              ]
            ]
          ]

    getLabel attrs = case [lbl | Label lbl <- attrs] of
                       [] -> ""
                       (StrLabel l:_) -> T.unpack l
                       (l:_) -> show l

main :: IO ()
main = do
  [path, out] <- getArgs
  graph <- parseDot path
  runX $ root [] [graphXml graph >>> uniqueNamespacesFromDeclAndQNames] >>> propagateNamespaces >>> writeDocument [withCheckNamespaces yes, withIndent yes] out
  return ()


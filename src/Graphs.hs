module Graphs where

import Data.Tree
import Data.Tree.Zipper
import Data.Graph.Inductive

treeToGraph :: (Graph gr) => Tree a -> gr a ()
treeToGraph tr = labelTree tr [1..]

labelTree tr = let tr1 = fmap tr (\x -> (x,0))
                   tp1 = fromTree tr1
                   tp2 = go 0 tp1
                   tr2 = toTree tp2
                         

                   go ix tp = let tp' = modifyLabel (\(l,_) -> (l,ix)) tp in
                                

                   go tp = case next tp of
                             Nothing -> firstChild tp
                             Just tpN -> go tpN

labelTree (Node a xs) (y:ys) = let (xs',ys') = labelTree' xs ys
                                   n' = Node (a,y) xs'
                               in
                                   (n',xs')

labelTree' lst labs = 
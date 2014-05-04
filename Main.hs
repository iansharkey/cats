import Control.Monad.State
import Data.Set (Set, empty)

  
data Node a = Node a
              deriving Show

data Graph a = Graph (Set (Node a)) (Set (Int, Int)) (Maybe Int)
               deriving Show

newtype StringN = StringN String
                  deriving Show



class EquivClass a where
  generate :: a -> [a]

class Splittable a where
  split :: Node a -> Maybe (Node a, Node a)

class Combinable a where
  combine :: Node a -> Node a -> Node a

class SetTransform a where
  transformSets :: (Set a, Set a) -> State Int (Set a, Set a)

class SeqTransform a where
  transformSeq :: EquivClass a => [a] -> State Int [a]


{-
gen n s = gen' s (n-1) s
gen' _ 0 accum  = accum
gen' s n accum  = gen' s (n-1) [ x++y | x <- accum, y <- s ]
-}

gen n s = foldl (\s accum -> [ x++y | x<-accum, y<-s]) s $ replicate (n-1) s

instance EquivClass StringN where
  generate (StringN s) = map StringN $ gen (length s) [ [x] | x <- ['A'..'Z']]
  
instance SeqTransform StringN where
  transformSeq seqs = mapM generateNext seqs
                      where generateNext s = do
                              cur <- get
                              put (cur+1)
                              return $ (generate s) !! cur


toSeq :: Graph a -> [a]
toSeq graph@(Graph nodes edges (Just rootIndex)) = depthFirst graph rootIndex
toSeq graph = depthFirst graph 0

depthFirst :: Graph a -> Int -> [a]
depthFirst graph rootIndex = depthFirst' graph rootIndex empty []

depthFirst' graph@(Graph nodes edges _) rootIndex visited accum = undefined
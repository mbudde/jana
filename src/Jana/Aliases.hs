module Jana.Aliases where

import Data.Set
import Data.Maybe (catMaybes)


type AliasSet = Set (String, String)

uniquePairs [] = []
uniquePairs (x:xs) = [ (x, y) | y <- xs ] ++ uniquePairs xs

introduce :: [(String, String)] -> AliasSet
introduce xs =
  findDuplicates (uniquePairs xs)
  where findDuplicates [] = empty
        findDuplicates (((x, x'), (y, y')):xs) =
          (if x == y then insert (x', y') else id) $ findDuplicates xs

propagate :: [(String, String)] -> AliasSet -> AliasSet
propagate xs aliases = Data.Set.foldr b empty aliases
  where b (x, y) ys = fromList [ (a, b) | (x', a) <- xs
                                        , (y', b) <- xs
                                        , x == x' && y == y' ]
                      `union` ys


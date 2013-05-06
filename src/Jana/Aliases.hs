module Jana.Aliases (
  AliasSet,
  empty,
  introduce, propagate, mergeAliases,
  isAlias
  ) where

import Data.Maybe (catMaybes)
import qualified Data.Set as Set

import Jana.Ast


type AliasSet = Set.Set (String, String)

empty :: AliasSet
empty = Set.empty

uniquePairs [] = []
uniquePairs (x:xs) = [ (x, y) | y <- xs ] ++ uniquePairs xs

introduce :: [(String, String)] -> AliasSet
introduce xs =
  findDuplicates (uniquePairs xs)
  where findDuplicates [] = empty
        findDuplicates (((x, x'), (y, y')):xs) =
          (if x == y then Set.insert (x', y') else id) $ findDuplicates xs

propagate :: [(String, String)] -> AliasSet -> AliasSet
propagate xs aliases = Set.foldr b empty aliases
  where b (x, y) ys = Set.fromList [ (a, b) | (x', a) <- xs
                                            , (y', b) <- xs
                                            , x == x' && y == y' ]
                      `Set.union` ys

mergeAliases :: [(String, String)] -> AliasSet -> AliasSet
mergeAliases xs aliases =
  introduce xs `Set.union` propagate xs aliases


isAlias :: AliasSet -> String -> String -> Bool
isAlias aliases id1 id2 =
  id1 == id2 ||
  (id1, id2) `Set.member` aliases ||
  (id2, id1) `Set.member` aliases

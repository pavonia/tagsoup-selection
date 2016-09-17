{-# LANGUAGE CPP #-}

{- |
    A zipper for TagSoup 'TagTree's.
-}
module Text.HTML.TagSoup.Tree.Zipper (
    TagTreePos (..),
    fromTagTree,

    root,
    parent,
    firstChild,
    lastChild,
    prevSibling,
    nextSibling,

    iteratePos,

    traverseTree,
    traverseTreeBF
) where


import           Control.Applicative
import           Control.Monad

import           Data.List
import           Data.Monoid
import           Data.Sequence (Seq, (|>), (<|), ViewL (..), viewl)
import qualified Data.Sequence as Seq

import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Tree



#if MIN_VERSION_base(4,5,0)
#else
infixr 6 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif



data TagTreePos str = TagTreePos {
        content :: TagTree str,
        before  :: [TagTree str],
        after   :: [TagTree str],
        parents :: [([TagTree str], str, [Attribute str], [TagTree str])]
    } deriving (Show)


fromTagTree :: TagTree str -> TagTreePos str
fromTagTree t = TagTreePos t [] [] []


root :: TagTreePos str -> TagTreePos str
root pos = last (pos : iteratePos parent pos)


parent :: TagTreePos str -> Maybe (TagTreePos str)
parent pos = case pos of
    TagTreePos _ _ _ [] -> Nothing
    TagTreePos t lcs rcs ((ls, n, as, rs):ps) -> Just TagTreePos {
            content = TagBranch n as $ reverse lcs ++ [t] ++ rcs,
            before  = ls,
            after   = rs,
            parents = ps
        }


firstChild :: TagTreePos str -> Maybe (TagTreePos str)
firstChild pos@(TagTreePos t lcs rcs ps) = case t of
    TagLeaf _             -> Nothing
    TagBranch n as []     -> Nothing
    TagBranch n as (t:ts) -> Just TagTreePos {
            content = t,
            before  = [],
            after   = ts,
            parents = (lcs, n, as, rcs) : ps
        }


lastChild :: TagTreePos str -> Maybe (TagTreePos str)
lastChild pos@(TagTreePos t lcs rcs ps) = case t of
    TagLeaf _         -> Nothing
    TagBranch n as [] -> Nothing
    TagBranch n as ts -> let (t : ts') = reverse ts in Just TagTreePos {
            content = t,
            before  = ts',
            after   = [],
            parents = (lcs, n, as, rcs) : ps
        }


prevSibling :: TagTreePos str -> Maybe (TagTreePos str)
prevSibling pos@(TagTreePos t lcs rcs ps) = case lcs of
    []     -> Nothing
    l : ls -> Just TagTreePos {
            content = l,
            before  = ls,
            after   = t : rcs,
            parents = ps
        }


nextSibling :: TagTreePos str -> Maybe (TagTreePos str)
nextSibling pos@(TagTreePos t lcs rcs ps) = case rcs of
    []     -> Nothing
    r : rs -> Just TagTreePos {
            content = r,
            before  = t : lcs,
            after   = rs,
            parents = ps
        }

{- |
    @iteratePos iter pos@ applies @iter@ to @pos@ until 'Nothing' is returned, collecting all
    new positions in the result list.
-}
iteratePos :: (TagTreePos str -> Maybe (TagTreePos str)) -> TagTreePos str -> [TagTreePos str]
iteratePos iter = unfoldr $ (join (,) <$>) . iter


{- |
    @traverseTree f pos@ performs a depth-first traversal of a tree. The starting position is
    @pos@, and the result is obtained from the values produced by applying @f@ to each visited
    node. Note that this function will also traverse parent nodes if the position isn't
    indicating the root of the tree.
-}
traverseTree :: Monoid m => (TagTreePos str -> m) -> TagTreePos str -> m
traverseTree f pos =
    f pos <> maybe mempty (traverseTree f) (firstChild pos <|> nextSibling pos <|> nextParent pos)
    where
        nextParent = parent >=> (\pos -> nextSibling pos <|> nextParent pos)


-- | Like 'traverseTree', but performs a breadth-first traversal.
traverseTreeBF :: Monoid m => (TagTreePos str -> m) -> TagTreePos str -> m
traverseTreeBF f = traverse . Seq.singleton
    where
        toSeq = maybe Seq.empty Seq.singleton
        traverse cs = case viewl cs of
            EmptyL    -> mempty
            pos :< ps -> f pos <> traverse (toSeq (nextSibling pos) <> ps <> toSeq (firstChild pos))

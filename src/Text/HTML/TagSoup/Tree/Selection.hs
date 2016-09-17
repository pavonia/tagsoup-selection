{- |
    Selecting subtrees from TagSoup 'TagTree's.
-}
module Text.HTML.TagSoup.Tree.Selection (
    selectGroup,
    select,
    selectAll,

    canMatch,

    matchesWith
) where


import Control.Applicative
import Control.Category ((>>>))

import Data.Maybe

import Text.CSS3.Selectors.Syntax

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.HTML.TagSoup.Tree.Util
import Text.HTML.TagSoup.Tree.Zipper

import Text.StringLike
import Text.StringLike.Matchable


{- |
    Traverses a tree, collecting all subtree positions for which at least one matches on the
    given selector group. If at least one subtree matches, a list of the same length as the
    number of selectors in the given selector group is added to the result list. In order,
    a 'Nothing' indicates no match and 'Just' a match for the corresponding selector.
-}
selectGroup :: Matchable str => SelectorGroup -> TagTree str -> [[Maybe (TagTreePos str)]]
selectGroup (SelectorGroup sel sels) = map (\(pos, bs) -> map (\b -> if b then Just pos else Nothing) bs)
    . filter (or . snd) . selectAll (sel : sels)

-- | Traverses a tree, collecting all subtree positions which match on the given selector.
select :: Matchable str => Selector -> TagTree str -> [TagTreePos str]
select sel = map fst . filter (head . snd) . selectAll [sel]

{- |
    Traverses a tree, collecting all traversed positions in order. For each position all
    selectors are tried to be matched with the node at the current position and the information
    about whether a match is possible is added to the result list.
-}
selectAll :: Matchable str => [Selector] -> TagTree str -> [(TagTreePos str, [Bool])]
selectAll sels = traverseTree (\pos -> [(pos, map (`canMatch` pos) sels)]) . fromTagTree

-- | Checks if a selector can be matched with the node at the given position.
canMatch :: Matchable str => Selector -> TagTreePos str -> Bool
canMatch sel = (&&) <$> simpleSelSeqPred s <*> matches ss (map combPosList cs)
    where
        (s : ss, cs) = chain sel

        combPosList :: Combinator -> TagTreePos str -> [TagTreePos str]
        combPosList c = case c of
            Descendant      -> iteratePos parent
            Child           -> take 1 . iteratePos parent
            AdjacentSibling -> take 1 . iteratePos prevSibling
            GeneralSibling  -> iteratePos prevSibling

        matches :: Matchable str => [SimpleSelectorSequence] -> [TagTreePos str -> [TagTreePos str]] -> TagTreePos str -> Bool
        matches ss cfs = foldr (\(s, cf) rest -> any ((&&) <$> simpleSelSeqPred s <*> rest) . cf) (const True) (zip ss cfs)


chain :: Selector -> ([SimpleSelectorSequence], [Combinator])
chain sel = let (seqs, combs) = chain' sel in (reverse seqs, reverse combs)
    where
        chain' (Selector seq mbComb) = case mbComb of
            Nothing           -> (seq : [], [])
            Just (comb, seq') -> let (seqs, combs) = chain' seq'
                                 in (seq : seqs, comb : combs)


simpleSelSeqPred :: Matchable str => SimpleSelectorSequence -> TagTreePos str -> Bool
simpleSelSeqPred (SimpleSelectorSequence head tails) = foldr1 (liftA2 (&&)) $ hFilter head : map tFilter tails
    where
        hFilter :: Matchable str => HeadSimpleSelector -> TagTreePos str -> Bool
        hFilter sel = case sel of
            TypeSelector n    -> content >>> hasTagBranchName (fromString n)
            UniversalSelector -> content >>> isTagBranch

        tFilter :: Matchable str => TailSimpleSelector -> TagTreePos str -> Bool
        tFilter sel = case sel of
            AttributeSelector a Nothing        -> content >>> hasTagBranchAttr (fromString a)
            AttributeSelector a (Just (op, v)) -> content >>> findTagBranchAttr (fromString a)
                                                          >>> maybe False (matchesWith op $ fromString v)
            ClassSelector v                    -> content >>> findTagBranchAttr (fromString "class")
                                                          >>> maybe False (matchesWith IncludesMatch $ fromString v)
            IDSelector v                       -> content >>> findTagBranchAttr (fromString "id")
                                                          >>> maybe False (matchesWith ExactMatch $ fromString v)
            PseudoClass p                      -> pseudo p

        pseudo :: Matchable str => PseudoClass -> TagTreePos str -> Bool
        pseudo p = case p of
            Root            -> parents >>> null

            -- pattern: a*n+b; counting starts at one; a==0 => match b-th; n always >= 0; only a*n+b >= 0 count
            NthChild p      -> error "simpleSelSeqPred: nth-child not implemented yet"
            NthLastChild p  -> error "simpleSelSeqPred: nth-last-child not implemented yet"
            NthOfType p     -> error "simpleSelSeqPred: nth-of-type not implemented yet"
            NthLastOfType p -> error "simpleSelSeqPred: nth-last-of-type not implemented yet"

            FirstChild      -> (&&) <$> (parents >>> not . null) <*> (before >>> null)
            LastChild       -> (&&) <$> (parents >>> not . null) <*> (after >>> null)
            FirstOfType     -> \pos -> let name = tagBranchName $ content pos
                                       in (&&) <$> (parents >>> not . null)
                                               <*> (before >>> all (not . hasTagBranchName name)) $ pos
            LastOfType      -> \pos -> let name = tagBranchName $ content pos
                                       in (&&) <$> (parents >>> not . null)
                                               <*> (after >>> all (not . hasTagBranchName name)) $ pos
            OnlyChild       -> (&&) <$> pseudo FirstChild <*> pseudo LastChild
            OnlyOfType      -> (&&) <$> pseudo FirstOfType <*> pseudo LastOfType
            Empty           -> content >>> children >>> null
            Not (Right (PseudoClass (Not _))) -> error "simpleSelSeqPred: nested :not pseudo classes"
            Not (Left sel)  -> not . hFilter sel
            Not (Right sel) -> not . tFilter sel


-- | Checks if two string-like values match under the given attribute operator.
matchesWith :: Matchable str => AttributeOperator -> str -> str -> Bool
matchesWith op = case op of
    ExactMatch    -> matchesExactly
    IncludesMatch -> matchesWordOf
    DashMatch     -> \v s -> v `matchesExactly` s || (v `append` fromChar '-') `matchesPrefixOf` s
    PrefixMatch   -> matchesPrefixOf
    SuffixMatch   -> matchesSuffixOf
    InfixMatch    -> matchesInfixOf

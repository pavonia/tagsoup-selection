{- |
    Utility functions for TagSoup's 'TagTree'.
-}
module Text.HTML.TagSoup.Tree.Util (
    children,
    descendants,

    withTagTree,
    isTagBranch,
    isTagLeaf,

    tagBranchName,
    tagBranchAttrs,
    tagBranchTrees,

    fromTagLeaf,

    maybeTagBranchName,
    maybeTagBranchAttrs,
    maybeTagBranchTrees,
    maybeTagLeafTag,

    hasTagBranchName,
    hasTagBranchAttr,
    findTagBranchAttr,

    tagTree',
    htmlRoot
) where


import Control.Applicative

import Data.Char
import Data.Maybe
import Data.List

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree

import Text.StringLike



-- | Returns all immediate children of a 'TagTree'.
children :: TagTree str -> [TagTree str]
children t = case t of
    TagBranch _ _ ts -> ts
    TagLeaf _        -> []

-- | Returns all immediate children, the children of these children etc. for a 'TagTree'.
descendants :: TagTree str -> [TagTree str]
descendants t = case t of
    TagBranch _ _ ts -> concatMap (\t -> t : descendants t) ts
    TagLeaf _        -> []


-- | Case analysis for 'TagTree' values.
withTagTree :: (str -> [Attribute str] -> [TagTree str] -> a) -> (Tag str -> a) -> TagTree str -> a
withTagTree fBr fLf t = case t of
    TagBranch n as ts -> fBr n as ts
    TagLeaf t         -> fLf t

-- | Checks if the given tree is a 'TagBranch'.
isTagBranch :: TagTree str -> Bool
isTagBranch = withTagTree (\_ _ _ -> True) (const False)

-- | Checks if the given tree is a 'TagLeaf'.
isTagLeaf :: TagTree str -> Bool
isTagLeaf = withTagTree (\_ _ _ -> False) (const True)


-- | Returns the element name of a 'TagBranch', or an error if the tree is a 'TagLeaf'.
tagBranchName :: TagTree str -> str
tagBranchName t = case t of
    TagBranch n _ _ -> n
    TagLeaf _       -> error "tagBranchName: TagLeaf"

-- | Returns the attribute list of a 'TagBranch', or an error if the tree is a 'TagLeaf'.
tagBranchAttrs :: TagTree str -> [Attribute str]
tagBranchAttrs t = case t of
    TagBranch _ as _ -> as
    TagLeaf _        -> error "tagBranchAttrs: TagLeaf"

-- | Returns the subtrees of a 'TagBranch', or an error if the tree is a 'TagLeaf'.
tagBranchTrees :: TagTree str -> [TagTree str]
tagBranchTrees t = case t of
    TagBranch _ _ ts -> ts
    TagLeaf _        -> error "tagBranchTrees: TagLeaf"


-- | Returns the inner 'Tag' value if the tree is a 'TagLeaf', or an error otherwise.
fromTagLeaf :: TagTree str -> Tag str
fromTagLeaf = withTagTree (\_ _ _ -> error "fromTagLeaf: TagBranch") id


maybeTagBranchName :: TagTree str -> Maybe str
maybeTagBranchName = withTagTree (\n _ _ -> Just n) (const Nothing)

maybeTagBranchAttrs :: TagTree str -> Maybe [Attribute str]
maybeTagBranchAttrs = withTagTree (\_ as _ -> Just as) (const Nothing)

maybeTagBranchTrees :: TagTree str -> Maybe [TagTree str]
maybeTagBranchTrees = withTagTree (\_ _ ts -> Just ts) (const Nothing)

maybeTagLeafTag :: TagTree str -> Maybe (Tag str)
maybeTagLeafTag = withTagTree (\_ _ _ -> Nothing) Just


hasTagBranchName :: Eq str => str -> TagTree str -> Bool
hasTagBranchName n = withTagTree (\n' _ _ -> n == n') (const False)

hasTagBranchAttr :: Eq str => str -> TagTree str -> Bool
hasTagBranchAttr = fmap isJust . findTagBranchAttr

findTagBranchAttr :: Eq str => str -> TagTree str -> Maybe str
findTagBranchAttr a = withTagTree (\_ as _ -> lookup a as) (const Nothing)


{- |
    An alternative 'tagTree' version. The original version sometimes yields unsatisfying results
    if the tag soup includes several spurious opening or closing tags. This version tries harder
    to balance them properly.
-}
tagTree' :: StringLike str => [Tag str] -> [TagTree str]
tagTree' tags = (pre ++) . finish . foldl' (flip step) ([], [[]]) $ post
    where
        -- DOCTYPE cleanup (see <http://www.w3.org/TR/html-markup/documents.html#conformant-documents>)
        (pre, post) = case break ((||) <$> isTagOpen <*> isTagClose) tags of
                          (pre, o@(TagOpen n _) : post)
                              | toString n == "!DOCTYPE" -> (map TagLeaf $ pre ++ [o], post)
                          _ -> ([], tags)

finish :: StringLike str => ([Tag str], [[TagTree str]]) -> [TagTree str]
finish (os, tss) = case (os, tss) of
    ([], [out])                            -> reverse out
    (TagOpen no as : os', ts : ts' : tss') -> finish (os', (TagBranch no as (reverse ts) : ts') : tss')
    _ -> error ": mismatching number of opening tags and subtrees"


step :: StringLike str => Tag str -> ([Tag str], [[TagTree str]]) -> ([Tag str], [[TagTree str]])
step tag (os, tss) = case tag of
    TagOpen _ _ -> (tag : os, [] : tss)
    TagClose nc -> case os of
        -- If last opened tag matches, remove it and add branch to last context
        TagOpen no as : os'
            | no == nc -> push os' (tail tss) $ TagBranch no as $ reverse $ head tss

        -- If previous opened matches, add spurious opened to context and close branch
        o@(TagOpen no as) : TagOpen no' as' : os'
            | no' == nc -> let ts : ts' : tss' = tss
                           in  push os' tss' $ TagBranch no' as' $ reverse $ ts ++ TagLeaf o : ts'

        -- Otherwise treat as spurious closed
        _ -> push os tss $ TagLeaf tag
    _ -> push os tss $ TagLeaf tag
    where
        push os (ts : tss) t = (os, (t : ts) : tss)
        push _  _          _ = error "push: empty context stack"


{- |
    Tries to find the @html@ branch in a 'TagTree'. If no branch with that name exists, a new
    @html@ branch is returned with the input trees as its immediate children.
-}
htmlRoot :: [TagTree String] -> TagTree String
htmlRoot tags = case break isTagBranch tags of
    (_, t@(TagBranch n _ _) : _)
        | map toLower (toString n) == "html" -> t
    _ -> TagBranch "html" [] tags

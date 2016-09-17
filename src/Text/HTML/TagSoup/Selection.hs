{- |
    This module re-exports most of the modules and functions for selecting subtrees from
    TagSoup's 'TagTree' type.
-}
module Text.HTML.TagSoup.Selection (
    module Text.CSS3.Selectors.Syntax,
    parseSelectorGroup,
    parseSelector,

    module Text.HTML.TagSoup.Tree.Selection,
    module Text.HTML.TagSoup.Tree.Util,
    module Text.HTML.TagSoup.Tree.Zipper    
) where

import Text.CSS3.Selectors.Parser (parseSelectorGroup, parseSelector)
import Text.CSS3.Selectors.Syntax

import Text.HTML.TagSoup.Tree.Selection
import Text.HTML.TagSoup.Tree.Util
import Text.HTML.TagSoup.Tree.Zipper

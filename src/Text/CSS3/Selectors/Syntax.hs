{- |
    Syntax of CSS3 selectors.
-}
module Text.CSS3.Selectors.Syntax (
    SelectorGroup (..),
    Selector (..),

    SimpleSelectorSequence (..),
    HeadSimpleSelector (..),
    TailSimpleSelector (..),
    Combinator (..),
    AttributeOperator (..),
    PseudoClass (..),
    PseudoClassParameter (..),

    Specificity (..),
    specificity
) where


import Data.List



-- * Syntax of CSS3 Selectors

-- | > sel [, sel ...]
data SelectorGroup = SelectorGroup Selector [Selector]
                     deriving (Show, Read, Eq)

data Selector = Selector SimpleSelectorSequence (Maybe (Combinator, Selector))
                deriving (Show, Read, Eq)

data SimpleSelectorSequence = SimpleSelectorSequence HeadSimpleSelector [TailSimpleSelector]
                              deriving (Show, Read, Eq)

data HeadSimpleSelector = TypeSelector String  -- ^ > type
                        | UniversalSelector    -- ^ > *
                          deriving (Show, Read, Eq)

data TailSimpleSelector = AttributeSelector String (Maybe (AttributeOperator, String))
                          -- ^ @[attr]@ or @[attr=value]@
                        | ClassSelector String     -- ^ > .class
                        | IDSelector String        -- ^ > #id
                        | PseudoClass PseudoClass  -- ^ > :pseudo-class
                          deriving (Show, Read, Eq)

data Combinator = Descendant       -- ^ > E F
                | Child            -- ^ > E > F 
                | AdjacentSibling  -- ^ > E + F
                | GeneralSibling   -- ^ > E ~ F
                  deriving (Show, Read, Eq)

data AttributeOperator = ExactMatch     -- ^ > A =  V
                       | IncludesMatch  -- ^ > A ~= V
                       | DashMatch      -- ^ > A |= V
                       | PrefixMatch    -- ^ > A ^= V
                       | SuffixMatch    -- ^ > A $= V
                       | InfixMatch     -- ^ > A *= V
                         deriving (Show, Read, Eq)

data PseudoClass = Root
                 | NthChild PseudoClassParameter
                 | NthLastChild PseudoClassParameter
                 | NthOfType PseudoClassParameter
                 | NthLastOfType PseudoClassParameter
                 | FirstChild
                 | LastChild
                 | FirstOfType
                 | LastOfType
                 | OnlyChild
                 | OnlyOfType
                 | Empty
                 | Not (Either HeadSimpleSelector TailSimpleSelector)
                   -- ^ Note: According to the specs, @:not@ pseudo-classes may not be nested.
                   --   The syntax doesn't reflect that.
                   deriving (Show, Read, Eq)

data PseudoClassParameter = PseudoClassParameter Integer Integer
                            deriving (Show, Read, Eq)


-- * Specificity of Selectors

{- |
    The specificity of a selector with its components decreasing in significance from left to
    right. The components in @Specificity a b c@ denote

        * @a@: the number of ID selectors,

        * @b@: the number of class selectors, attribute selectors and pseudo-classes (not
               counting the @:not@ pseudo-class itself), and

        * @c@: the number of type selectors and pseudo-elements

    as described in <http://www.w3.org/TR/selectors/#specificity>.
-}
data Specificity = Specificity !Int !Int !Int deriving (Show, Read, Eq, Ord)

-- | Returns the specificity for a selector.
specificity :: Selector -> Specificity
specificity (Selector (SimpleSelectorSequence hSel tSels) mbComb) =
    maybe spec ((spec .+) . specificity . snd) mbComb
    where
        spec = foldl1' (.+) $ hSpec hSel : map tSpec tSels
        hSpec h = case h of
                      TypeSelector _    -> Specificity 0 0 1
                      UniversalSelector -> Specificity 0 0 0
        tSpec t = case t of
                      AttributeSelector _ _  -> Specificity 0 1 0
                      ClassSelector _        -> Specificity 0 1 0
                      IDSelector _           -> Specificity 1 0 0
                      PseudoClass (Not eSel) -> either hSpec tSpec eSel
                      PseudoClass _          -> Specificity 0 1 0
        (.+) (Specificity a b c) (Specificity a' b' c') = Specificity (a + a') (b + b') (c + c')

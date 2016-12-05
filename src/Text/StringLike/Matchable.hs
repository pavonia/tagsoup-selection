{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Text.StringLike.Matchable (
    Matchable (..),

    checkNull
) where

import Data.List
import Text.StringLike
import qualified Data.Text as Text.Strict
import qualified Data.Text.Lazy as Text.Lazy


{- |
    An extention of string-like types for sub-string matching at various postions.
-}
class StringLike str => Matchable str where

    -- | Checks if two strings match exactly. This should by equal to '(==)' for most types.
    matchesExactly  :: str -> str -> Bool
    matchesExactly = (==)

    -- | Checks if the first string is a prefix of of the second. If the first string is empty, the
    --   result is always 'False'.
    matchesPrefixOf :: str -> str -> Bool

    -- | Checks if the first string is an infix of the second, i.e. if the first string appears
    --   somewhere in the second. If the first string is empty, the result is always 'False'.
    matchesInfixOf  :: str -> str -> Bool

    -- | Checks if the first string is a suffix of the second. If the first string is empty, the
    --   result is always 'False'.
    matchesSuffixOf :: str -> str -> Bool

    -- | Checks if the first string matches any of the whitespace-separated substrings in the second
    --   string exactly. If the first string is empty, the result is always 'False'.
    matchesWordOf   :: str -> str -> Bool


instance Matchable String where
    matchesPrefixOf = checkNull null isPrefixOf
    matchesInfixOf  = checkNull null isInfixOf
    matchesSuffixOf = checkNull null isSuffixOf
    matchesWordOf   = \s -> any (`matchesExactly` s) . splitOn (`elem` " \t\n\r")


instance Matchable Text.Strict.Text where
    matchesPrefixOf = checkNull Text.Strict.null Text.Strict.isPrefixOf
    matchesInfixOf  = checkNull Text.Strict.null Text.Strict.isInfixOf
    matchesSuffixOf = checkNull Text.Strict.null Text.Strict.isSuffixOf
    matchesWordOf   = \s -> any (`matchesExactly` s) . Text.Strict.split (`elem` " \t\n\r")


instance Matchable Text.Lazy.Text where
    matchesPrefixOf = checkNull Text.Lazy.null Text.Lazy.isPrefixOf
    matchesInfixOf  = checkNull Text.Lazy.null Text.Lazy.isInfixOf
    matchesSuffixOf = checkNull Text.Lazy.null Text.Lazy.isSuffixOf
    matchesWordOf   = \s -> any (`matchesExactly` s) . Text.Lazy.split (`elem` " \t\n\r")


-- | @checkNull null comp s1 s2@ returns 'False' if either @null s1 == True@ or @comp s1 s2 == False@,
--   and returns 'True' otherwise.
checkNull :: (s -> Bool) -> (s -> s -> Bool) -> (s -> s -> Bool)
checkNull null comp s1 s2 = not (null s1) && comp s1 s2


splitOn :: (Char -> Bool) -> String -> [String]
splitOn pred s = case break pred $ dropWhile pred s of
    ("", "") -> []
    (w, s')  -> w : splitOn pred s'

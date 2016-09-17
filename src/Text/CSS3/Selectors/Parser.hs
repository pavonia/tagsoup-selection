{- |
    Parser functions for CSS3 selectors.
-}
module Text.CSS3.Selectors.Parser (
    -- * Parsing Functions
    parseSelectorGroup,
    parseSelector,
    sel,

    -- * CSS Selector Parsers
    selectorGroup,
    selector,
    simpleSelectorSequence,
    headSimpleSelector,
    tailSimpleSelector,
    attributeSelector,
    attributeOperator,
    combinator,
    pseudoClass,
    pseudoClassParameter,

    -- * Lexical Syntax Parsers
    whitespace,
    identifier,
    name,
    nameStartLetter,
    nameCharLetter
) where


import           Control.Applicative
import           Control.Monad

import           Data.Char
import           Data.Maybe

import           Numeric

import           Text.CSS3.Selectors.Syntax
import           Text.Parsec hiding ((<|>), optional, newline, many, Empty)
import qualified Text.Parsec as Parsec ((<|>), optional, newline, many)
import           Text.Parsec.String


{-
  * http://www.w3.org/TR/selectors/
  * http://www.w3.org/TR/css-syntax-3/
-}

{-
Selector                ::= SimpleSelectorSequence [PseudoElement]
                         |  SimpleSelectorSequence Combinator Selector
SimpleSelectorSequence  ::= (TypeSelector | UniversalSelector)
                            (AttributeSelector | ClassSelector | IDSelector | PseudoClass)*
                         |  (AttributeSelector | ClassSelector | IDSelector | PseudoClass)+
SimpleSelector          ::= (TypeSelector | UniversalSelector | AttributeSelector | ClassSelector | IDSelector | PseudoClass)
Combinator              ::= [WhiteSpace] (' ' | '>' | '+' | '~') [WhiteSpace]
WhiteSpace              ::= ' ' | '\t' | '\n' | '\r' | '\f'
Group                   ::= Selector | Selector [WhiteSpace] ',' [WhiteSpace] Group

TypeSelector            ::= identifier
UniversalSelector       ::= '*'
AttributeSelector       ::= '[' Attribute ']'
                         |  '[' Attribute AttributeOperator AttributeValue ']'
Attribute               ::= identifier | String
AttributeOperator       ::= '=' | '~=' | '|=' | '^=' | '$=' | '*='

IDSelector              ::= '#' identifier
ClassSelector           ::= '.' AttributeValue
PseudoClass             ::= ':' (StructuralPseudoClass | 'not(' SimpleSelector ')')
StructuralPseudoClass   ::= 'root'
                         |  'nth-child' PseudoClassParameter
                         |  'nth-last-child' PseudoClassParameter
                         |  'nth-of-type' PseudoClassParameter
                         |  'nth-last-of-type' PseudoClassParameter
                         |  'first-child'
                         |  'last-child'
                         |  'first-of-type'
                         |  'last-of-type'
                         |  'only-child'
                         |  'only-of-type'
                         |  'empty'
-}


-- * Parsing Functions

-- | Tries to parse a selector group.
parseSelectorGroup :: String -> Either ParseError SelectorGroup
parseSelectorGroup = parse (selectorGroup <* eof) ""

-- | Tries to parse a single selector.
parseSelector :: String -> Either ParseError Selector
parseSelector = parse (selector <* eof) ""


-- | Parses a single selector and fails with an error if the string cannot be parsed correctly.
--   This function is intended for testing purposes only.
sel :: String -> Selector
sel = either (error . show) id . parse (selector <* eof) ""


-- * CSS Selector Parsers

selectorGroup :: Parser SelectorGroup
selectorGroup = SelectorGroup <$> selector
                              <*> many (many whitespace *> char ',' *> many whitespace *> selector)


selector :: Parser Selector
selector = Selector <$> simpleSelectorSequence
                    <*> optional (try $ (,) <$> combinator <*> selector)


simpleSelectorSequence :: Parser SimpleSelectorSequence
simpleSelectorSequence =  SimpleSelectorSequence <$> headSimpleSelector <*> many tailSimpleSelector
                      <|> SimpleSelectorSequence UniversalSelector      <$> many1 tailSimpleSelector


headSimpleSelector :: Parser HeadSimpleSelector
headSimpleSelector =  TypeSelector      <$> identifier
                  <|> UniversalSelector <$  char '*'


tailSimpleSelector :: Parser TailSimpleSelector
tailSimpleSelector  =  attributeSelector
                   <|> ClassSelector <$> (char '.' *> identifier)
                   <|> IDSelector    <$> (char '#' *> name)
                   <|> PseudoClass   <$> (char ':' *> pseudoClass)

attributeSelector :: Parser TailSimpleSelector
attributeSelector = AttributeSelector
    <$> (char '[' *> many whitespace *> identifier <* many whitespace)
    <*> optional ((,) <$> attributeOperator <* many whitespace
                      <*> (identifier <|> quotedString) <* many whitespace)
    <*  char ']'

attributeOperator :: Parser AttributeOperator
attributeOperator  =  ExactMatch    <$ string "="
                  <|> IncludesMatch <$ string "~="
                  <|> DashMatch     <$ string "|="
                  <|> PrefixMatch   <$ string "^="
                  <|> SuffixMatch   <$ string "$="
                  <|> InfixMatch    <$ string "*="


combinator :: Parser Combinator
combinator =  try (Child           <$ (many whitespace *> char '>' <* many whitespace))
          <|> try (AdjacentSibling <$ (many whitespace *> char '+' <* many whitespace))
          <|> try (GeneralSibling  <$ (many whitespace *> char '~' <* many whitespace))
          <|>      Descendant      <$ many1 whitespace


pseudoClass :: Parser PseudoClass
pseudoClass  =  Root          <$   try (string "root")
            <|> NthChild      <$> (try (string "nth-child")        *> parameter pseudoClassParameter)
            <|> NthLastChild  <$> (try (string "nth-last-child")   *> parameter pseudoClassParameter)
            <|> NthOfType     <$> (try (string "nth-of-type")      *> parameter pseudoClassParameter)
            <|> NthLastOfType <$> (try (string "nth-last-of-type") *> parameter pseudoClassParameter)
            <|> FirstChild    <$   try (string "first-child")
            <|> LastChild     <$   try (string "last-child")
            <|> FirstOfType   <$   try (string "first-of-type")
            <|> LastOfType    <$   try (string "last-of-type")
            <|> OnlyChild     <$   try (string "only-child")
            <|> OnlyOfType    <$   try (string "only-of-type")
            <|> Empty         <$   try (string "empty")
            <|> Not           <$> (try kwNot *> parameter (Left <$> headSimpleSelector
                                                      <|> Right <$> tailSimpleSelector))
    where parameter p = char '(' *> many whitespace *> p <* many whitespace <* char ')'

{-
nth : S* [ ['-'|'+']? INTEGER? {N} [ S* ['-'|'+'] S* INTEGER ]?
         | ['-'|'+']? INTEGER
         | {O}{D}{D}
         | {E}{V}{E}{N} ] S*
-}
pseudoClassParameter :: Parser PseudoClassParameter
pseudoClassParameter  =  PseudoClassParameter     <$> (try $ option id sign <*> option 1 integer <* oneOf "nN")
                                                  <*> (option 0 $ many whitespace *> sign <* many whitespace <*> integer)
                     <|> PseudoClassParameter 0   <$> (option id sign <*> integer)
                     <|> PseudoClassParameter 2 1 <$  kwOdd
                     <|> PseudoClassParameter 2 0 <$  kwEven



-- * Lexical Syntax Parsers
-- According to <https://www.w3.org/TR/css3-selectors/#w3cselgrammar>

sign :: Parser (Integer -> Integer)
sign = negate <$ char '-' <|> id <$ char '+'

-- integer = [0-9]+
integer :: Parser Integer
integer = foldl (\a c -> 10*a + toInteger (ord c) - 48) 0 <$> many1 digit

-- num       [0-9]+|[0-9]*\.[0-9]+


-- identifier = [-]?{nameStartLetter}{nameCharLetter}*
identifier :: Parser String
identifier = (\d c cs -> (maybe id (:) d) $ c : cs)
    <$> optional (char '-') <*> nameStartLetter <*> many nameCharLetter

-- name = {nameCharLetter}+
name :: Parser String
name = many1 nameCharLetter

-- nameStartLetter = [_a-z]|{nonAsciiLetter}|{escapedLetter}
nameStartLetter :: Parser Char
nameStartLetter = asciiLetter <|> char '_' <|> nonAsciiLetter <|> escapedLetter

-- nameCharLetter = [_a-z0-9-]|{nonAsciiLetter}|{escapedLetter}
nameCharLetter :: Parser Char
nameCharLetter = char '_' <|> asciiLetter <|> digit <|> char '-' <|> nonAsciiLetter <|> escapedLetter

-- asciiLatter = [_a-z]
asciiLetter :: Parser Char
asciiLetter = satisfy $ \c -> c >= 'a' && c <= 'z' || c >='A' && c <= 'Z'

-- nonAsciiLetter = [\240-\377]  -- CSS 2.1
-- nonAsciiLetter = [^\0-\177]   -- CSS 3
nonAsciiLetter :: Parser Char
nonAsciiLetter = satisfy (\c -> ord c > 127)

-- unicodeLetter = \\[0-9a-f]{1,6}(\r\n|[ \n\r\t\f])?
unicodeLetter :: Parser Char
unicodeLetter = char '\\' *> (chr . fst . head . readHex <$> countFromTo 1 6 (satisfy isHexDigit))
                          <* optional (try (string "\r\n") <|> pure <$> whitespace)

-- escapedLetter = {unicode}|\\[^\n\r\f0-9a-f]
escapedLetter :: Parser Char
escapedLetter  =  try unicodeLetter
              <|> char '\\' *> satisfy (\c -> not (isHexDigit c) && c `notElem` "\n\r\f")


{-
quotedString = {string1}|{string2}
string1 = \"([^\n\r\f\\"]|\\{newline}|{nonAsciiLetter}|{escapedLetter})*\"
string2 = \'([^\n\r\f\\']|\\{newline}|{nonAsciiLetter}|{escapedLetter})*\'
-}
quotedString :: Parser String
quotedString  =  char '\"' *> (concat <$> many (stringChar '\"')) <* char '\"'
             <|> char '\'' *> (concat <$> many (stringChar '\'')) <* char '\''
    where
        stringChar :: Char -> Parser String
        stringChar delim  =  pure <$> satisfy (`notElem` (delim : "\n\r\f\\"))
                         <|> try (char '\\' *> newline)
--                         <|> pure <$> nonAsciiLetter  -- Can this ever be reached?
                         <|> pure <$> escapedLetter

-- newline = \n|\r\n|\r|\f
newline :: Parser String
newline = try (string "\r\n") <|> pure <$> oneOf "\n\r\f"

-- whitespace = [ \t\r\n\f]*
whitespace :: Parser Char
whitespace = oneOf " \t\n\r\f"

{-
D         d|\\0{0,4}(44|64)(\r\n|[ \t\r\n\f])?
E         e|\\0{0,4}(45|65)(\r\n|[ \t\r\n\f])?
N         n|\\0{0,4}(4e|6e)(\r\n|[ \t\r\n\f])?|\\n
O         o|\\0{0,4}(4f|6f)(\r\n|[ \t\r\n\f])?|\\o
T         t|\\0{0,4}(54|74)(\r\n|[ \t\r\n\f])?|\\t
V         v|\\0{0,4}(58|78)(\r\n|[ \t\r\n\f])?|\\v
-}
kwNot, kwOdd, kwEven :: Parser String
kwNot  = sequence [oneOf "nN", oneOf "oO", oneOf "tT"]
kwOdd  = sequence [oneOf "oO", oneOf "dD", oneOf "dD"]
kwEven = sequence [oneOf "eE", oneOf "vV", oneOf "eE", oneOf "nN"]


-- * Utility Parsers

countFromTo :: Stream s m t => Int -> Int -> ParsecT s u m a -> ParsecT s u m [a]
countFromTo n m p
    | m < n     = pure []
    | otherwise = (++) <$> count n p <*> (catMaybes <$> replicateM (m - n) (optional p))

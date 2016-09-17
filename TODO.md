# TODO

## Parser

* Pseudo-elements (<http://www.w3.org/TR/selectors/#pseudo-elements>)
* Characters in selectors can be escaped with a backslash according to the same escaping rules as CSS (<http://www.w3.org/TR/CSS21/syndata.html#characters>)
* Namespaces are not supported yet
* `[att~=val]` Represents an element with the att attribute whose value is a *whitespace*-separated list of words
* Attribute values must be CSS identifiers or strings. [CSS21] The case-sensitivity of attribute names and values in selectors depends on the document language.
* If an element has multiple ID attributes, all of them must be treated as IDs for that element for the purposes of the ID selector.
* Pseudo-class names are case-insensitive
* Dynamic pseudo-classes (<http://www.w3.org/TR/selectors/#dynamic-pseudos>)
* <http://www.w3.org/TR/CSS21/syndata.html#characters>
* Ignore comments
* Token stream preprocessing (<http://www.w3.org/TR/css-syntax-3/#input-preprocessing>)
* Escaped characters for `odd`/`even`/`not`
* CSS 4
    - [Draft](https://drafts.csswg.org/selectors-4/#relational)
    - [Latest version](http://www.w3.org/TR/selectors4/)


## StringLike

* Add instances for
    - `Data.ByteString.ByteString`
    - `Data.ByteString.Lazy.ByteString`
    - `Data.Text.Text`
    - `Data.Text.Lazy.Text`


## Selection

* Handle case-insensitivity (<http://www.w3.org/TR/selectors/#casesens>)
* Select `nth` functions (<http://www.w3.org/TR/selectors/#structural-pseudos>)


## `tagTree'`

* HTML-Repair
    - <http://tidy.sourceforge.net/>
    - <https://www.w3.org/TR/DOM-Parsing/#parsing>
    - Test: `<html lang="en-US"><body><div></header></div></body></html>`
* Take inline/block nestings rules into account
* Add option to remove spurious opening/closing tags, and convert them to proper branch
* Handle self-closing elements (see <https://hackage.haskell.org/package/hexpat-tagsoup-0.1/docs/src/Text-XML-Expat-TagSoup.html#parseTags>)

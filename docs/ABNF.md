# ABNF Input Format

ABNF is an alternative input format for instaparse grammar specifications.  ABNF does not provide any additional expressive power over instaparse's default EBNF-based syntax, so if you are new to instaparse and parsing, you do not need to read this document -- stick with the syntax described in [the tutorial](https://github.com/Engelberg/instaparse/blob/master/README.md).

ABNF's main virtue is that it is precisely specified and commonly used in protocol specifications.  If you use such protocols, instaparse's ABNF input format is a simple way to turn the ABNF specification into an executable parser.  However, unless you are working with such specifications, you do not need the ABNF input format.

## EBNF vs ABNF

### EBNF

The most common notation for expressing context-free grammars is [Backus-Naur Form](http://en.wikipedia.org/wiki/Backus%E2%80%93Naur_Form), or BNF for short.  BNF, however, is a little too simplistic.  People wanted more convenient notation for expressing repetitions, so [EBNF](http://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_Form), or *Extended* Backus-Naur Form was developed.

There is a hodge-podge of various syntax extensions that all fall under the umbrella of EBNF.  For example, one standard specifies that repetitions should be specified with `{}`, but regular expression operators such as `+`, `*`, and `?` are far more popular.

When creating the primary input format for instaparse, I based the syntax off of EBNF.  I consulted various standards I found on the internet, and filtered it through my own experience of what I've seen in various textbooks and specs over the years.  I included the official repetition operators as well as the ones derived from regular expressions.  I also incorporated PEG-like syntax extensions.

What I ended up with was a slightly tweaked version of EBNF, making it relatively easy to turn any EBNF-specified grammar into an executable parser.  However, with multiple competing standards and actively-used variations, there's no guarantee that an EBNF grammar that you find will perfectly align with instaparse's syntax.  You may need to make a few tweaks to get it to work.

### ABNF

From what I can tell, the purpose of [ABNF](http://en.wikipedia.org/wiki/Augmented_Backus%E2%80%93Naur_Form), or *Augmented* Backus-Naur Form, was to create a grammar syntax that would have a single, well-defined, formal standard, so that all ABNF grammars would look exactly the same.

For this reason, ABNF seems to be a more popular grammar syntax in the world of specifications and protocols.  For example, if you want to know the formal definition of what constitutes a valid URI, there's an ABNF grammar for that.

After instaparse's initial release, I received a couple requests to support ABNF as an alternative input format.  Since ABNF is so precisely defined, in theory, any ABNF grammar should work without modification.  In practice, I've found that many ABNF specifications have one or two small typos; nevertheless, applying instaparse to ABNF is mostly a trivial copy-paste exercise.

I included whatever further extensions and extra instaparse goodies I could safely include, but omitted any extension that would conflict with the ABNF standard and jeopardize the ability to use ABNF grammar specifications without modification.

Aside from just wanting to adhere to the ABNF specifcation, I can think of a few niceties that ABNF provides over EBNF:

1. ABNF has a convenient syntax for specifying bounded repetitions, for example, something like "between 3 and 5 repetitions of the letter a".

2. Convenient syntax for expressing characters and ranges of characters.

3. ABNF comes with a "standard library" of a dozen or so common token rules.

## Usage

To get a feeling for what ABNF syntax looks like, first check out this [ABNF specification for phone URIs.](https://raw.github.com/Engelberg/instaparse/master/test/instaparse/phone_uri.txt)  I copied and pasted it directly from the formal spec -- found one typo which I fixed.

	(def phone-uri-parser
	  (insta/parser "https://raw.github.com/Engelberg/instaparse/abnf/test/instaparse/phone_uri.txt"
	          :input-format :abnf))

	=> (phone-uri-parser "tel:+1-201-555-0123")
	[:TELEPHONE-URI
	 "tel:"
	 [:TELEPHONE-SUBSCRIBER
	  [:GLOBAL-NUMBER
	   [:GLOBAL-NUMBER-DIGITS
	    "+"
	    [:DIGIT "1"]
	    [:PHONEDIGIT [:VISUAL-SEPARATOR "-"]]
	    [:PHONEDIGIT [:DIGIT "2"]]
	    [:PHONEDIGIT [:DIGIT "0"]]
	    [:PHONEDIGIT [:DIGIT "1"]]
	    [:PHONEDIGIT [:VISUAL-SEPARATOR "-"]]
	    [:PHONEDIGIT [:DIGIT "5"]]
	    [:PHONEDIGIT [:DIGIT "5"]]
	    [:PHONEDIGIT [:DIGIT "5"]]
	    [:PHONEDIGIT [:VISUAL-SEPARATOR "-"]]
	    [:PHONEDIGIT [:DIGIT "0"]]
	    [:PHONEDIGIT [:DIGIT "1"]]
	    [:PHONEDIGIT [:DIGIT "2"]]
	    [:PHONEDIGIT [:DIGIT "3"]]]]]]

The usage, as you can see, is almost identical to the way you build parsers using the `insta/parser` constructor.  The only difference is the additional keyword argument `:input-format :abnf`.

If you find yourself working with a whole series of ABNF parser specifications, you may find it more convenient to call

	(insta/set-default-input-format! :abnf)

to alter the default input format.  Changing the default makes it unnecessary to specify `:input-format :abnf` with each call to the parser constructor.

Here is the doc string:

	=> (doc insta/set-default-input-format!)
	-------------------------
	instaparse.core/set-default-input-format!
	([type])
	  Changes the default input format.  Input should be :abnf or :ebnf

## ABNF Syntax Guide

<table>
<tr><th>Category</th><th>Notations</th><th>Example</th></tr>
<tr><td>Rule</td><td>= /=</td><td>S = A</td></tr>
<tr><td>Alternation</td><td>/</td><td>A / B</td></tr>
<tr><td>Concatenation</td><td>whitespace</td><td>A B</td></tr>
<tr><td>Grouping</td><td>()</td><td>(A / B) C</td></tr>
<tr><td>Bounded Repetition</td><td>*</td><td>3*5 A</td></tr>
<tr><td>Optional</td><td>*1</td><td>*1 A</td></tr>
<tr><td>One or more</td><td>1*</td><td>1* A</td></tr>
<tr><td>Zero or more</td><td>*</td><td>*A</td></tr>
<tr><td>String terminal</td><td>"" ''</td><td>'a' "a"</td></tr>
<tr><td>Regex terminal</td><td>#"" #''</td><td>#'a' #"a"</td></tr>
<tr><td>Character terminal</td><td>%d %b %x</td><td>%x30-37</td></tr>
<tr><td>Comment</td><td>;</td><td>; comment to the end of the line</td></tr>
<tr><td>Lookahead</td><td>&</td><td>&A</td></tr>
<tr><td>Negative lookahead</td><td>!</td><td>!A</td></tr>
</table>

Some important things to be aware of:

+ According to the ABNF standard, all strings are *case-insensitive*.
+ ABNF strings do not support any kind of escape characters.  Use ABNF's character notation to specify unusual characters.
+ In ABNF, there is one repetition operator, `*`, and it *precedes* the thing that it is operating on.  So, for example, 3*5 means "between 3 and 5 repetitions".  The first number defaults to 0 and the second defaults to infinity, so you can omit one or both numbers to get effects comparable to EBNF's `+`, `*`, and `?`.  `4*4` could just be written as `4`.
+ Use `;` for comments to the end of the line.  The ABNF specification has rigid definitions about where comments can be, but in instaparse the rules for comment placement are a bit more flexible and intuitive.
+ ABNF uses `/` for the ordinary alternative operator with no order implied.
+ ABNF allows the restatement of a rule name to specify multiple alternatives.  The custom is to use `/=` in definitions that are adding alternatives, for example `S = 'a' / 'b'` could be written as:

<br>

	S = 'a'
	S /= 'b'

## Extensions

Instaparse extends ABNF by allowing single-quoted strings and both double-quoted and single-quoted regular expressions.  The PEG extensions of lookahead `&` and negative lookahead `!` are permitted, but the PEG extension of ordered choice could not be included because of the syntactic conflict with ABNF's usage of `/` for unordered alternatives.

Instaparse is somewhat more flexible with whitespace than the ABNF specification dictates, but somewhat less flexible than you might expect from the EBNF input format.  For example, in instaparse's EBNF mode, `(A B)C` would be just fine, but ABNF insists on at least one space to indicate concatenation, so you'd have to write `(A B) C`.  I relaxed whitespace restrictions when I could do so without radically deviating from the specification.

### Angle brackets

The ABNF input format supports instaparse's angle bracket notation, where angle brackets can be used to hide certain parts of the grammar from the resulting tree structure.  Including instaparse's angle bracket notation was a bit of a tough decision because technically angle brackets are reserved for special use in ABNF grammars.

However, in ABNF notation, angle brackets are meant to be used for prose descriptions of some concept that can't be mechanically specified in the grammar.  For example:

	P = <a prime number>

I realized that such constructs can't be mechanically handled anyway, so I might as well co-opt the angle bracket notation, as I did with the EBNF syntax, for the very handy purpose of hiding.

This means that when you paste in an ABNF specification, it is always wise to do a quick scan to make sure that no angle brackets were used.  They are rarely used, but one [notably strange use of angle brackets](http://w3-org.9356.n7.nabble.com/ipath-empty-ABNF-rule-td192464.html) occurs in the URI specification, which uses `0<ipchar>` to designate the empty string.  So be aware of these sorts of possibilities, but you're unlikely to run into them.

## The standard rules

The ABNF specification states that the following rules are always available for use in ABNF grammars:

<table>
<tr><th>Name</th><th>Explanation</th></tr>
<tr><td>ALPHA</td><td>Alphabetic character</td></tr>
<tr><td>BIT</td><td>0 or 1</td></tr>
<tr><td>CHAR</td><td>ASCII character</td></tr>
<tr><td>CR</td><td>\r</td></tr>
<tr><td>CRLF</td><td>\r\n</td></tr>
<tr><td>CTL</td><td>control character</td></tr>
<tr><td>DIGIT</td><td>0-9</td></tr>
<tr><td>DQUOTE</td><td>"</td></tr>
<tr><td>HEXDIG</td><td>Hexadecimal digit: 0-9 or A-F</td></tr>
<tr><td>HTAB</td><td>\t</td></tr>
<tr><td>LF</td><td>\n</td></tr>
<tr><td>LWSP</td><td>A specific mixture of whitespace and CRLF (see note below)</td></tr>
<tr><td>OCTET</td><td>8-bit character</td></tr>
<tr><td>SP</td><td>the space character</td></tr>
<tr><td>VCHAR</td><td>visible character</td></tr>
<tr><td>WSP</td><td>space or tab</td></tr>
</table>

LWSP is particularly quirky, defined to be either a space or tab character, or an alternating sequence of carriage-return-linefeed and a single space or tab character.  It's very specific, presumably relevant to some particular protocol, but not generally useful and I don't recommend using it.

## Combinators

The `instaparse.combinators` contains a few combinators that are not documented in the main tutorial, but are listed here because they are only relevant to ABNF grammars.

<table>
<tr><th>String syntax</th><th>Combinator</th><th>Mnemonic</th></tr>
<tr><td>"abc" (as used in ABNF)</td><td>string-ci</td><td>string, case-insensitive</td></tr>
<tr><td>3*5 (as used in ABNF)</td><td>rep</td><td>repetition</td></tr>
</table>

Finally, just as there exists an `ebnf` function in the combinators namespace that turns EBNF fragments into combinator-built data structures, there exists an `abnf` function which does the same for ABNF fragments.

This means it is entirely possible to take fragments of EBNF syntax along with fragments of ABNF syntax, and convert all the pieces, merging them into a grammar map along with other pieces built from combinators.  I don't expect that many people will need this ability to mix and match, but it's there if you need it.
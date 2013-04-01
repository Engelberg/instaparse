# Instaparse

*What if context-free grammars were as easy to use as regular expressions?*

## Features

Instaparse aims to be the simplest way to build parsers in Clojure.

+ Turns *standard EBNF notation* for context-free grammars into an executable parser that takes a string as an input and produces a parse tree for that string.
+ Works for *any* context-free grammar, including *left-recursive*, *right-recursive*, and *ambiguous* grammars.  Instaparse's unofficial motto: *No Grammar Left Behind*.
+ Extends the power of context-free grammars with PEG-like syntax for lookahead and negative lookahead.
+ Supports both of Clojure's most popular tree formats (hiccup and enlive) as an output target.
+ Detailed reporting of parse errors.
+ Optionally produces lazy sequence of all parses (especially useful for diagnosing and debugging ambiguous grammars).
+ "Total parsing" mode where leftover string is embedded in the parse tree
+ Optional combinator library for building grammars programmatically.
+ Performant

## Quickstart

Instaparse requires Clojure v1.5.1 or later.  (It may work with earlier versions, but its speed relies extensively on features new to 1.5.)

Add the following line to your leiningen dependencies:

	[instaparse "1.0.0"]

Require instaparse in your namespace header:

	(ns example.core
	  (:require [instaparse.core :as insta])

## Tutorial

### Creating your first parser

Here's a typical example of a context-free grammar one might see in a textbook on automata and/or parsing:

	S = AB*
	AB = A B
	A = 'a'+
	B = 'b'+

This looks for alternating runs of 'a' followed by runs of 'b'.  So for example "aaaaabbaaabbb" satisfies this grammar.  On the other hand,
"aaabbbbaa" does not (because the grammar specifies that each run of 'a' must be followed by a run of 'b').

With instaparse, turning this grammar into an executable parser is as simple as typing the grammar in:

	(def as-and-bs
	  (insta/parser
	    "S = AB*
	     AB = A B
	     A = 'a'+
	     B = 'b'+"))

	=> (as-and-bs "aaaaabbbaaaabb")
	[:S
	 [:AB [:A "a" "a" "a" "a" "a"] [:B "b" "b" "b"]]
	 [:AB [:A "a" "a" "a" "a"] [:B "b" "b"]]]

### Notation

Instaparse supports most of the common notations for context-free grammars.  For example, a popular alternative to `*` is to surround the term with curly braces `{}`, and a popular alternative to `?` is to surround the term with square brackets `[]`.  Rules can be specified with `=`, `:`, `:=`, or `::=`.  Rules can optionally end with `;`.  Instaparse is very flexible in terms of how you use whitespace (as in Clojure, `,` is treated as whitespace) and you can liberally use parentheses for grouping.  Terminal strings can be enclosed in either single quotes or double quotes (however, since you are writing the grammar specification inside of a Clojure double-quoted string, any use of double-quotes would have to be escaped, therefore single-quotes are easier to read). Newlines are optional; you can put the entire grammar on one line if you desire.  In fact, all these notations can be mixed up in the same specification if you want.

So here is an equally valid (but messier) way to write out the exact same grammar, just to illustrate the flexibility that you have:

	(def as-and-bs-alternative
	  (insta/parser
	    "S:={AB}  ;
	     AB ::= (A, B)
	     A : \"a\" + ;
	     B ='b' + ;"))

Note that regardless of the notation you use in your specification, when you evaluate the parser at the REPL, the rules will be pretty-printed:

	=> as-and-bs-alternative
	S = AB*
	AB = A B
	A = "a"+
	B = "b"+

Here's a quick guide to the syntax for defining context-free grammars:

<table>
<tr><th>Category</th><th>Notations</th><th>Example</th></tr>
<tr><td>Rule</td><td>: := ::= =</td><td>S = A</td></tr>
<tr><td>Alternation</td><td>|</td><td>A | B</td></tr>
<tr><td>Concatenation</td><td>whitespace</td><td>A B</td></tr>
<tr><td>Grouping</td><td>()</td><td>(A | B) C</td></tr>
<tr><td>Optional</td><td>? []</td><td>A? [A]</td></tr>
<tr><td>One or more</td><td>+</td><td>A+</td></tr>
<tr><td>Zero or more</td><td>* {}</td><td>A* {A}</td></tr>
<tr><td>String terminal</td><td>"" ''</td><td>'a' "a"</td></tr>
<tr><td>Regex terminal</td><td>#"" #''</td><td>#'a' #"a"</td></tr>
<tr><td>Epsilon</td><td>Epsilon epsilon EPSILON eps &#949; "" ''</td><td>S = 'a' S | Epsilon</td></tr>
</table>

As is the norm in EBNF notation, concatenation has a higher precedence than alternation, so in the absence of parentheses, something like `A B | C D` means `(A B) | (C D)`.

### Input from resource file

Parsers can also be built from a specification contained in a file, either locally or on the web.  For example, I stored on github a file with a simple grammar to parse text containing a single 'a' surrounded optionally by whitespace.  The specification in the file looks like this:

	S = #"\s*" "a" #"\s*"

Building the parser from the URI is easy:

	(insta/parser "https://gist.github.com/Engelberg/5283346/raw/77e0b1d0cd7388a7ddf43e307804861f49082eb6/SingleA")

This provides a convenienent way to share parser specifications over the Internet.

### Escape characters

Putting your grammar in a separate resource file has an additional advantage -- it provides a very straightforward "what you see is what you get" view of the grammar.  The only escape characters needed are the ordinary escape characters for strings and regular expressions (additionally, instaparse also supports `\'` inside single-quoted strings).

When you specify a grammar directly in your Clojure code as a double-quoted string, extra escape characters may be needed in the strings and regexes of your grammar:

1. All `"` string and regex delimiters must be turned into `\"` or replaced with a single-quote `'`.
2. All backslash characters in your strings and regexes `\` should be escaped and turned into `\\`.  (In some cases you can get away with not escaping the backslash, but it is best practice to always do it.)

For example, the above grammar could be written in Clojure as:
	(insta/parser "S = #'\\s*' 'a' #'\\s*'")

It is unfortunate that this extra level of escaping is necessary.  Many programming languages provide some sort of facility for creating "raw strings" which are taken verbatim (e.g., Python's triple-quoted strings).  I don't understand why Clojure does not support raw strings, but it doesn't.

Fortunately, for many grammars this is a non-issue, and if the escaping does get bad enough to affect readability, there is always the option of storing the grammar in a separate file.

### Output format

When building parsers, you can specify an output format of either :hiccup or :enlive.  :hiccup is the default, but here is an example of the above parser with :enlive set as the output format:

	(def as-and-bs-enlive
	  (insta/parser
	    "S = AB*
	     AB = A B
	     A = 'a'+
	     B = 'b'+"
	    :output-format :enlive))

	=> (as-and-bs-enlive "aaaaabbbaaaabb")
	{:tag :S,
	 :content
	 ({:tag :AB,
	   :content
	   ({:tag :A, :content ("a" "a" "a" "a" "a")}
	    {:tag :B, :content ("b" "b" "b")})}
	  {:tag :AB,
	   :content
	   ({:tag :A, :content ("a" "a" "a" "a")}
	    {:tag :B, :content ("b" "b")})})}

I find the hiccup format to be pleasant and compact, especially when working with the parsed output in the REPL.  The main advantage of the enlive format is that it allows you to use the very powerful enlive library to select and transform nodes in your tree.

If you want to alter instaparse's default output format:

	(insta/set-default-output-format! :enlive)

### Controlling the tree structure

The principles of instaparse's output trees:
- Every rule equals one level of nesting in the tree.
- Each level is automatically tagged with the name of the rule.

To better understand this, take a look at these two variations of the same parser we've been discussing:

	(def as-and-bs-variation1
	  (insta/parser
	    "S = AB*
	     AB = 'a'+ 'b'+"))

	=> (as-and-bs-variation1 "aaaaabbbaaaabb")
	[:S
	 [:AB "a" "a" "a" "a" "a" "b" "b" "b"]
	 [:AB "a" "a" "a" "a" "b" "b"]]

	(def as-and-bs-variation2
	  (insta/parser
	    "S = ('a'+ 'b'+)*"))

	=> (as-and-bs-variation2 "aaaaabbbaaaabb")
	[:S "a" "a" "a" "a" "a" "b" "b" "b" "a" "a" "a" "a" "b" "b"]

#### Hiding content

For this next example, let's consider a parser that looks for a sequence of a's or b's surrounded by parens.

	(def paren-ab
	  (insta/parser
	    "paren-wrapped = '(' seq-of-A-or-B ')'
	     seq-of-A-or-B = ('a' | 'b')*"))

	=> (paren-ab "(aba)")
	[:paren-wrapped "(" [:seq-of-A-or-B "a" "b" "a"] ")"]

It's very common in parsers to have elements that need to be present in the input and parsed, but we'd rather not have them appear in the output.    In the above example, the parens are essential to the grammar yet the tree would be much easier to read and manipulate if we could hide those parens; once the string has been parsed, the parens themselves carry no additional semantic value.

In instaparse, you can use angle brackets `<>` to hide parsed elements, suppressing them from the tree output.

	(def paren-ab-hide-parens
	  (insta/parser
	    "paren-wrapped = <'('> seq-of-A-or-B <')'>
	     seq-of-A-or-B = ('a' | 'b')*"))

	=> (paren-ab-hide-parens "(aba)")
	[:paren-wrapped [:seq-of-A-or-B "a" "b" "a"]]

Voila! The parens "(" and ")" tokens have been hidden.  Angle brackets are a powerful tool for hiding whitespace and other delimiters from the output.

#### Hiding tags

Continuing with the same example parser, let's say we decide that the :seq-of-A-or-B tag is also superfluous -- we'd rather not have that extra nesting level appear in the output tree.

We've already seen that one option is to simply lift the right-hand side of the seq-of-A-or-B rule into the paren-wrapped rule, as follows:

	(def paren-ab-manually-flattened
	  (insta/parser
	    "paren-wrapped = <'('> ('a'|'b')* <')'>"))

	=> (paren-ab-manually-flattened "(aba)")
	[:paren-wrapped "a" "b" "a"]

But sometimes, it is ugly or impractical to do this.  It would be nice to have a way to express the concept of "repeated sequence of a's and b's" as a separate rule, without necessarily introducing an additional level of nesting.

Again, the angle brackets come to the rescue.  We simply use the angle brackets to hide the *name* of the rule.  Since each name corresponds to a level of nesting, hiding the name means the parsed contents of that rule will appear in the output tree without the tag and its associated new level of nesting.

	(def paren-ab-hide-tag
	  (insta/parser
	    "paren-wrapped = <'('> seq-of-A-or-B <')'>
	     <seq-of-A-or-B> = ('a' | 'b')*"))

	=> (paren-ab-hide-tag "(aba)")
	[:paren-wrapped "a" "b" "a"]

### No Grammar Left Behind

One of the things that really sets instaparse apart from other Clojure parser generators is that it can handle any context-free grammar.  For example, some parsers only accept LL(1) grammars, others accept LALR grammars.  Many of the libraries use a recursive-descent strategy that fail for left-recursive grammars.  If you are willing to learn the esoteric restrictions posed by the library, it is usually possible to rework your grammar to fit that mold.  But instaparse lets you write your grammar in whatever way is most natural.

#### Right recursion

No problem:

	=> ((insta/parser "S = 'a' S | Epsilon") "aaaa")
	[:S "a" [:S "a" [:S "a" [:S "a" [:S]]]]]

Note the use of Epsilon, a common name for the "empty" parser that always succeeds without consuming any characters.  You can also just use an empty string if you prefer.

#### Left recursion

No problem:

	=> ((insta/parser "S = S 'a' | Epsilon") "aaaa")
	[:S [:S [:S [:S [:S] "a"] "a"] "a"] "a"]

As you can see, either of these recursive parsers will generate a parse tree that is deeply nested.  Unfortunately, Clojure does not handle deeply-nested data structures very well.  If you were to run the above parser on, say, a string of 20,000 a's, instaparse will happily try to generate the corresponding parse tree but then Clojure will stack overflow when it tries to hash the tree.

So, as is often the case in Clojure, use recursion judiciously in a way that will keep your trees a manageable depth.  For the above parser, it is almost certainly better to just do:

	=> ((insta/parser "S = 'a'*") "aaaa")
	[:S "a" "a" "a" "a"]
	
### Ambiguous grammars

	(def ambiguous
	  (insta/parser
	    "S = A A
	     A = 'a'*"))

This grammar is interesting because even though it specifies a repeated run of a's, there are many possible ways the grammar can chop it up.  Our parser will faithfully return one of the possible parses:

	=> (ambiguous "aaaaaa")
	[:S [:A "a"] [:A "a" "a" "a" "a" "a"]]

However, we can do better.  First, I should point out that `(ambiguous "aaaaaa")` is really just shorthand for `(insta/parse ambiguous "aaaaaa")`.  Parsers are not actually functions, but are records that implement the function interface as a shorthand for calling the insta/parse function.

`insta/parse` is the way you ask a parser to produce a single parse tree.  But there is another library function `insta/parses` that asks the parser to produce a lazy sequence of all parse trees.  Compare:
	=> (insta/parse ambiguous "aaaaaa")
	[:S [:A "a"] [:A "a" "a" "a" "a" "a"]]

	=> (insta/parses ambiguous "aaaaaa")
	([:S [:A "a"] [:A "a" "a" "a" "a" "a"]]
	 [:S [:A "a" "a" "a" "a" "a" "a"] [:A]]
	 [:S [:A "a" "a"] [:A "a" "a" "a" "a"]]
	 [:S [:A "a" "a" "a"] [:A "a" "a" "a"]]
	 [:S [:A "a" "a" "a" "a"] [:A "a" "a"]]
	 [:S [:A "a" "a" "a" "a" "a"] [:A "a"]]
	 [:S [:A] [:A "a" "a" "a" "a" "a" "a"]])

You may wonder, why is this useful?  Two reasons:
1. Sometimes it is difficult to remove ambiguity from a grammar, but the ambiguity doesn't really matter -- any parse tree will do.  In these situations, instaparse's ability to work with ambiguous grammars can be quite handy.
2. Instaparse's ability to generate a sequence of all parses provides a powerful tool for debugging and thus *removing* ambiguity from an unintentionally ambiguous grammar.  It turns out that when designing a context-free grammar, it's all too easy to accidentally introduce some unintentional ambiguity.  Other parser tools often report ambiguities as cryptic "shift-reduce" messages, if at all.  It's rather empowering to see the precise parse that instaparse finds when multiple parses are possible.

I generally test my parsers using the `insta/parses` function so I can immediately spot any ambiguities I've inadvertently introduced.  When I'm confident the parser is not ambiguous, I switch to `insta/parse` or just call the parser as a function.

### PEG extensions

### Error messages

### Alternative parsing modes

#### Partial parsing mode

#### Total parsing mode

## Reference

## Special Thanks

## Practitioner's notes
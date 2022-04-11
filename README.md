# Instaparse 1.4.10

*What if context-free grammars were as easy to use as regular expressions?*

## Features

Instaparse aims to be the simplest way to build parsers in Clojure.

+ Turns *standard EBNF or ABNF notation* for context-free grammars into an executable parser that takes a string as an input and produces a parse tree for that string.
+ *No Grammar Left Behind*: Works for *any* context-free grammar, including *left-recursive*, *right-recursive*, and *ambiguous* grammars.
+ Extends the power of context-free grammars with PEG-like syntax for lookahead and negative lookahead.
+ Supports both of Clojure's most popular tree formats (hiccup and enlive) as output targets.
+ Detailed reporting of parse errors.
+ Optionally produces lazy sequence of all parses (especially useful for diagnosing and debugging ambiguous grammars).
+ "Total parsing" mode where leftover string is embedded in the parse tree.
+ Optional combinator library for building grammars programmatically.
+ Performant.

## Quickstart

Instaparse requires Clojure v1.5.1 or later, or ClojureScript v1.7.28 or later.

Add the following line to your leiningen dependencies:

	[instaparse "1.4.10"]

Require instaparse in your namespace header:

	(ns example.core
	  (:require [instaparse.core :as insta]))

### Creating your first parser

Here's a typical example of a context-free grammar one might see in a textbook on automata and/or parsing.  It is a common convention in many textbooks to use the capital letter `S` to indicate the starting rule, so for this example, we'll follow that convention:

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

At this point, if you know EBNF notation for context-free grammars, you probably know enough to dive in and start playing around.  However, instaparse is rich with features, so if you want to know the full scope of what it can do, read on...

## Tutorial

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
<tr><td>End of rule</td><td>; . (optional)</td><td>S = A;</td></tr>
<tr><td>Alternation</td><td>|</td><td>A | B</td></tr>
<tr><td>Concatenation</td><td>whitespace or ,</td><td>A B</td></tr>
<tr><td>Grouping</td><td>()</td><td>(A | B) C</td></tr>
<tr><td>Optional</td><td>? []</td><td>A? [A]</td></tr>
<tr><td>One or more</td><td>+</td><td>A+</td></tr>
<tr><td>Zero or more</td><td>* {}</td><td>A* {A}</td></tr>
<tr><td>String terminal</td><td>"" ''</td><td>'a' "a"</td></tr>
<tr><td>Regex terminal</td><td>#"" #''</td><td>#'a' #"a"</td></tr>
<tr><td>Epsilon</td><td>Epsilon epsilon EPSILON eps &#949; "" ''</td><td>S = 'a' S | Epsilon</td></tr>
<tr><td>Comment</td><td>(* *)</td><td>(* This is a comment *)</td></tr>
</table>

As is the norm in EBNF notation, concatenation has a higher precedence than alternation, so in the absence of parentheses, something like `A B | C D` means `(A B) | (C D)`.

### Input from resource file

Parsers can also be built from a specification contained in a file, either locally or on the web.  For example, I stored on github a file with a simple grammar to parse text containing a single 'a' surrounded optionally by whitespace.  The specification in the file looks like this:

	S = #"\s*" "a" #"\s*"

Building the parser from the URI is easy:

	(insta/parser "https://gist.github.com/Engelberg/5283346/raw/77e0b1d0cd7388a7ddf43e307804861f49082eb6/SingleA")

This provides a convenienent way to share parser specifications over the Internet.

You can also use a specification contained in a local resource in your classpath:

	(insta/parser (clojure.java.io/resource "myparser.bnf"))

### `defparser`

On ClojureScript, the `(def my-parser (insta/parser "..."))` use case
has the following disadvantages:

- ClojureScript does not support `slurp`, so `parser` cannot automatically read from file paths / URLs.
- Having to parse a grammar string at runtime can impact the startup performance of an application or webpage.

To solve those problems, a macro `instaparse.core/defparser` is
provided that, if given a string for a grammar specification, will
parse that as a grammar up front and emit more performant code.

```clojure
;; Clojure
(:require [instaparse.core :as insta :refer [defparser]])
;; ClojureScript
(:require [instaparse.core :as insta :refer-macros [defparser]])

=> (time (def p (insta/parser "S = A B; A = 'a'+; B = 'b'+")))
"Elapsed time: 4.368179 msecs"
#'user/p
=> (time (defparser p "S = A B; A = 'a'+; B = 'b'+")) ; the meat of the work happens at macro-time
"Elapsed time: 0.091689 msecs"
#'user/p
=> (defparser p "https://gist.github.com/Engelberg/5283346/raw/77e0b1d0cd7388a7ddf43e307804861f49082eb6/SingleA") ; works even in cljs!
#'user/p
=> (defparser p [:S (c/plus (c/string "a"))]) ; still works, but won't do any extra magic behind the scenes
#'user/p
=> (defparser p "S = 1*'a'" :input-format :abnf :output-format :enlive) ; takes additional keyword arguments
#'user/p
```

`defparser` is primarily useful in Clojurescript, but works in both Clojure and Clojurescript for cross-platform compatibility.

### Escape characters

Putting your grammar in a separate resource file has an additional advantage -- it provides a very straightforward "what you see is what you get" view of the grammar.  The only escape characters needed are the ordinary escape characters for strings and regular expressions (additionally, instaparse also supports `\'` inside single-quoted strings).

When you specify a grammar directly in your Clojure code as a double-quoted string, extra escape characters may be needed in the strings and regexes of your grammar:

1. All `"` string and regex delimiters must be turned into `\"` or replaced with a single-quote `'`.

2. All backslash characters in your strings and regexes `\` should be escaped and turned into `\\`.  (In some cases you can get away with not escaping the backslash, but it is best practice to be consistent and always do it.)

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

You might wonder what would happen if we hid the root tag as well.  Let's take a look:

	(def paren-ab-hide-both-tags
	  (insta/parser
	    "<paren-wrapped> = <'('> seq-of-A-or-B <')'>
	     <seq-of-A-or-B> = ('a' | 'b')*"))

	=> (paren-ab-hide-both-tags "(aba)")
	("a" "b" "a")

With no root tag, the parser just returns a sequence of children.  So in the above example where *all* the tags are hidden, you just get a sequence of parsed elements.  Sometimes that's what you want, but in general, I recommend that you don't hide the root tag, ensuring the output is a well-formed tree.

#### Revealing hidden information

Sometimes, after setting up the parser to hide content and tags, you temporarily want to reveal the hidden information, perhaps for debugging purposes.

The optional keyword argument `:unhide :content` reveals the hidden content in the tree output.

	=> (paren-ab-hide-both-tags "(aba)" :unhide :content)
	("(" "a" "b" "a" ")")

The optional keyword argument `:unhide :tags` reveals the hidden tags in the tree output.

	=> (paren-ab-hide-both-tags "(aba)" :unhide :tags)
	[:paren-wrapped [:seq-of-A-or-B "a" "b" "a"]]

The optional keyword argument `:unhide :all` reveals all hidden information.

	=> (paren-ab-hide-both-tags "(aba)" :unhide :all)
	[:paren-wrapped "(" [:seq-of-A-or-B "a" "b" "a"] ")"]

### No Grammar Left Behind

One of the things that really sets instaparse apart from other Clojure parser generators is that it can handle any context-free grammar.  For example, some parsers only accept LL(1) grammars, others accept LALR grammars.  Many of the libraries use a recursive-descent strategy that fails for left-recursive grammars.  If you are willing to learn the esoteric restrictions posed by the library, it is usually possible to rework your grammar to fit that mold.  But instaparse lets you write your grammar in whatever way is most natural.

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

So, as is often advisable in Clojure, use recursion judiciously in a way that will keep your trees a manageable depth.  For the above parser, it is almost certainly better to just do:

	=> ((insta/parser "S = 'a'*") "aaaa")
	[:S "a" "a" "a" "a"]
	
#### Infinite loops

If you specify an unterminated recursive grammar, instaparse will handle that gracefully as well and terminate with an error, rather than getting caught in an infinite loop:

	=> ((insta/parser "S = S") "a")
	Parse error at line 1, column 1:
	a
	^

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

I generally test my parsers using the `insta/parses` function so I can immediately spot any ambiguities I've inadvertently introduced.  When I'm confident the parser is not ambiguous, I switch to `insta/parse` or, equivalently, just call the parser as if it were a function.

### Regular expressions: A word of warning

As you can see from the above example, instaparse flexibly interprets * and +, trying all possible numbers of repetitions in order to create a parse tree.  It is easy to become spoiled by this, and then forget that regular expressions have different semantics.  Instaparse's regular expressions are just Clojure/Java regular expressions, which behave in a greedy manner.

To better understand this point, contrast the above parser with this one:

	(def not-ambiguous
	  (insta/parser
	    "S = A A
	     A = #'a*'"))

	=> (insta/parses not-ambiguous "aaaaaa")
	([:S [:A "aaaaaa"] [:A ""]])

In this parser, the * is *inside* the regular expression, which means that it follows greedy regular expression semantics.  Therefore, the first A eats all the a's it can, leaving no a's for the second A.

For this reason, it is wise to use regular expressions judiciously, mainly to express the patterns of your tokens, and leave the overall task of parsing to instaparse.  Regular expressions can often be tortured and abused into serving as a crude parser, but don't do it!  There's no need; with instaparse, you now have an equally convenient but more expressive tool to bring to bear on parsing problems.

Here is an example that I think is a tasteful use of regular expressions to split a sentence on whitespaces, categorizing the tokens as words or numbers:

	(def words-and-numbers
	  (insta/parser
	    "sentence = token (<whitespace> token)*
	     <token> = word | number
	     whitespace = #'\\s+'
	     word = #'[a-zA-Z]+'
	     number = #'[0-9]+'"))

	=> (words-and-numbers "abc 123 def")
	[:sentence [:word "abc"] [:number "123"] [:word "def"]]

### Partial parses

By default, instaparse assumes you are looking for a parse tree that covers the entire input string.  However, sometimes it may be useful to look at all the partial parses that satisfy the grammar while consuming some initial portion of the input string.

For this purpose, both `insta/parse` and `insta/parses` take a keyword argument, `:partial` that you simply set to true.

	(def repeated-a
	  (insta/parser
	    "S = 'a'+"))

	=> (insta/parses repeated-a "aaaaaa")
	([:S "a" "a" "a" "a" "a" "a"])
	=> (insta/parses repeated-a "aaaaaa" :partial true)
	([:S "a"]
	 [:S "a" "a"]
	 [:S "a" "a" "a"]
	 [:S "a" "a" "a" "a"]
	 [:S "a" "a" "a" "a" "a"]
	 [:S "a" "a" "a" "a" "a" "a"])

Of course, using `:partial true` with `insta/parse` means that you'll only get the first parse result found.

	 => (insta/parse repeated-a "aaaaaa" :partial true)
	[:S "a"]

### PEG extensions

PEGs are a popular alternative to context-free grammars.  On the surface, PEGs look very similar to CFGs, but the various choice operators are meant to be interpreted in a strictly greedy, ordered way that removes any ambiguity from the grammar.  Some view this lack of ambiguity as an advantage, but it does limit the expressiveness of PEGs relative to context-free grammars.  Furthermore, PEGs are usually tightly coupled to a specific parsing strategy that forbids left-recursion, further limiting their utility.

To combat that lost expressiveness, PEGs adopted a few operators that actually allow PEGs to do some things that CFGs cannot express.  Even though the underlying paradigm is different, I've swiped these juicy bits from PEGs and included them in instaparse, giving instaparse more expressive power than either traditional PEGs or traditional CFGs.

Here is a table of the PEG operators that have been adapted for use in instaparse; I'll explain them in more detail shortly.

<table>
<tr><th>Category</th><th>Notations</th><th>Example</th></tr>
<tr><td>Lookahead</td><td>&</td><td>&A</td></tr>
<tr><td>Negative lookahead</td><td>!</td><td>!A</td></tr>
<tr><td>Ordered Choice</td><td>/</td><td>A / B</td></tr>
</table>

#### Lookahead

The symbol for lookahead is `&`, and is generally used as part of a chain of concatenated parsers.  Lookahead tests whether there are some number of characters that lie ahead in the text stream that satisfy the parser.  It performs this test without actually "consuming" characters.  Only if that lookahead test succeeds do the remaining parsers in the chain execute.

That's a mouthful, and hard to understand in the abstract, so let's look at a concrete example:

	(def lookahead-example
	  (insta/parser
	    "S = &'ab' ('a' | 'b')+"))

The `('a' | 'b')+` part should be familiar at this point, and you hopefully recognize this as a parser that ensures the text is a string entirely of a's and b's.  The other part, `&'ab'` is the lookahead.  Notice how the `&` precedes the expression it is operating on.  Before processing the `('a' | 'b')+`, it looks ahead to verify that the `'ab'` parser could hypothetically be satisfied by the upcoming characters.  In other words, it will only accept strings that start off with the characters `ab`.

	=> (lookahead-example "abaaaab")
	[:S "a" "b" "a" "a" "a" "a" "b"]
	=> (lookahead-example "bbaaaab")
	Parse error at line 1, column 1:
	bbaaaab
	^
	Expected:
	"ab"

If you write something like `&'a'+` with no parens, this will be interpreted as `&('a'+)`.

Here is my favorite example of lookahead, a parser that only succeeds on strings with a run of a's followed by a run of b's followed by a run of c's, where each of those runs must be the same length.  If you've ever taken an automata course, you may remember that there is a very elegant proof that it is impossible to express this set of constraints with a pure context-free grammar.  Well, with lookahead, it *is* possible:

	(def abc
	  (insta/parser
	    "S = &(A 'c') 'a'+ B
	     A = 'a' A? 'b'
	     <B> = 'b' B? 'c'"))

	=> (abc "aaabbbccc")
	[:S "a" "a" "a" "b" "b" "b" "c" "c" "c"]

This example succeeds because there are three a's followed by three b's followed by three c's.  Verifying that this parser fails for unequal runs and other mixes of letters is left as an exercise for the reader.

#### Negative lookahead

Negative lookahead uses the symbol `!`, and like `&`, it precedes the expression.  It does exactly what you'd expect -- it performs a lookahead and confirms that the parser is *not* satisfied by the upcoming characters in the screen.

	(def negative-lookahead-example
	  (insta/parser
	    "S = !'ab' ('a' | 'b')+"))

So this parser turns around the meaning of the previous example, accepting all strings of a's and b's that *don't* start off with `ab`.

	=> (negative-lookahead-example "abaaaab")
	Parse error at line 1, column 1:
	abaaaab
	^
	Expected:
	NOT "ab"
	=> (negative-lookahead-example "bbaaaab")
	[:S "b" "b" "a" "a" "a" "a" "b"]

One issue with negative lookahead is that it introduces the possibility of paradoxes.  Consider:

	S = !S 'a'

How should this parser behave on an input of "a"?  If S succeeds, it should fail, and if it fails it should succeed.

PEGs simply don't allow this sort of grammar, but the whole spirit of instaparse is to flexibly allow recursive grammars, so I needed to find some way to handle it.  Basically, I've taken steps to make sure that a paradoxical grammar won't cause instaparse to go into an infinite loop.  It will terminate, but I make no promises about what the results will be.  If you specify a paradoxical grammar, it's a garbage-in-garbage-out kind of situation (although to be clear, instaparse won't return complete garbage; it will make some sort of reasonable judgment about how to interpret it).  If you're curious about how instaparse behaves with the above paradoxical example, here it is:

	=> ((insta/parser "S = !S 'a'") "a")
	[:S "a"]

Negative lookahead, when used properly, is an extremely powerful tool for removing ambiguity from your parser.  To illustrate this, let's take a look at a very common parsing task, which involves tokenizing a string of characters into a combination of identifiers and reserved keywords.  Our first attempt at this ends up ambiguous:

	(def ambiguous-tokenizer
	  (insta/parser
	    "sentence = token (<whitespace> token)*
	     <token> = keyword | identifier
	     whitespace = #'\\s+'
	     identifier = #'[a-zA-Z]+'
	     keyword = 'cond' | 'defn'"))

	=> (insta/parses ambiguous-tokenizer "defn my cond")
	([:sentence [:identifier "defn"] [:identifier "my"] [:identifier "cond"]]
	 [:sentence [:keyword "defn"] [:identifier "my"] [:identifier "cond"]]
	 [:sentence [:identifier "defn"] [:identifier "my"] [:keyword "cond"]]
	 [:sentence [:keyword "defn"] [:identifier "my"] [:keyword "cond"]])

Each of our keywords not only fits the description of keyword, but also of identifier, so our parser doesn't know which way to parse those words.  Instaparse makes no guarantee about what order it processes alternatives, and in this situation, we see that in fact, the combination we wanted was listed last among the possible parses.  Negative lookahead provides an easy way to remove this ambiguity:

	(def unambiguous-tokenizer
	  (insta/parser
	    "sentence = token (<whitespace> token)*
	     <token> = keyword | !keyword identifier
	     whitespace = #'\\s+'
	     identifier = #'[a-zA-Z]+'
	     keyword = 'cond' | 'defn'"))

	=> (insta/parses unambiguous-tokenizer "defn my cond")
	([:sentence [:keyword "defn"] [:identifier "my"] [:keyword "cond"]])

#### Ordered choice

As I mentioned earlier, a PEG's interpretation of `+`, `*`, and `|` are subtly different from the way those symbols are interpreted in CFGs.  `+` and `*` are interpreted greedily, just as they are in regular expressions.  `|` proceeds in a rather strict order, trying the first alternative first, and only proceeding if that one fails.  To remind users that these multiple choices are strictly ordered, PEGs commonly use the forward slash `/` rather than `|`.

Although the PEG paradigm of forced order is antithetical to instaparse's flexible parsing strategy, I decided to co-opt the `/` notation to express a preference of one alternative over another.

With that in mind, let's look back at the `ambiguous-tokenizer` example from the previous section.  In that example, we found that our desired parse, in which the keywords were classified, ended up at the bottom of the heap:

	=> (insta/parses ambiguous-tokenizer "defn my cond")
	([:sentence [:identifier "defn"] [:identifier "my"] [:identifier "cond"]]
	 [:sentence [:keyword "defn"] [:identifier "my"] [:identifier "cond"]]
	 [:sentence [:identifier "defn"] [:identifier "my"] [:keyword "cond"]]
	 [:sentence [:keyword "defn"] [:identifier "my"] [:keyword "cond"]])

We've already seen one way to remove the ambiguity by using negative lookahead.  But now we have another tool in our toolbox, `/`, which will allow the ambiguity to remain, while bringing the desired parse result to the top of the list.

	(def preferential-tokenizer
	  (insta/parser
	    "sentence = token (<whitespace> token)*
	     <token> = keyword / identifier
	     whitespace = #'\\s+'
	     identifier = #'[a-zA-Z]+'
	     keyword = 'cond' | 'defn'"))

	=> (insta/parses preferential-tokenizer "defn my cond")
	([:sentence [:keyword "defn"] [:identifier "my"] [:keyword "cond"]]
	 [:sentence [:identifier "defn"] [:identifier "my"] [:keyword "cond"]]
	 [:sentence [:keyword "defn"] [:identifier "my"] [:identifier "cond"]]
	 [:sentence [:identifier "defn"] [:identifier "my"] [:identifier "cond"]])

The ordered choice operator has its uses, but don't go overboard.  There are two main reasons why it is generally better to use the regular unordered alternation operator.

1. When ordered choice interacts with a complex mix of recursion, other ordered choice operators, and indeterminate operators like `+` and `*`, it can quickly become difficult to reason about how the parsing will actually play out.

2. The next version of instaparse will support multithreading.  In that version, every use of `|` will be an opportunity to exploit parallelism.  On the contrary, uses of `/` will create a bottleneck where options have to be pursued in a specific order.

### Parse errors

`(insta/parse my-parser "parse this text")` will either return a parse tree or a failure object.  The failure object will pretty-print at the REPL, showing you the furthest point it reached while parsing your text, and listing all the possible tokens that would have allowed it to proceed.

`(insta/parses my-parser "parse this text")` will return a sequence of all the parse trees, so in the event that no parse can be found, it will simply return an empty list.  However, the failure object is still there, attached to the empty list as metadata.

`(insta/failure? result)` will detect both these scenarios and return true if the result is either a failure object, or an empty list with a failure object attached as metdata.

`(insta/get-failure result)` provides a unified way to extract the failure object in both these cases.  If the result is a failure object, then it is directly returned, and if the result is an empty list with the failure attached as metadata, then the failure object is retrieved from the metadata.

### Total parse mode

Sometimes knowing the point of failure is not enough and you need to know the entire context of the parse tree when it failed.  To help with these sorts of situations, instaparse offers a "total parse" mode inspired by Christophe Grand's parsley parser.  This total parse mode guarantees to parse the entire string; if the parser fails, it completes the parse anyway, embedding the failure point as a node in the parse tree.

To demonstrate, let's revisit the ultra-simple `repeated-a` parser.

	=> repeated-a
	S = "a"+
	=> (repeated-a "aaaaaaaa")
	[:S "a" "a" "a" "a" "a" "a" "a" "a"]

On a string with a valid parse, the total parse mode performs identically:

	=> (repeated-a "aaaaaaaa" :total true)
	[:S "a" "a" "a" "a" "a" "a" "a" "a"]

On a failure, note the difference:

	=> (repeated-a "aaaabaaa")
	Parse error at line 1, column 5:
	aaaabaaa
	    ^
	Expected:
	"a"
	=> (repeated-a "aaaabaaa" :total true)
	[:S "a" "a" "a" "a" [:instaparse/failure "baaa"]]

Note that this kind of total parse result is still considered a "failure", and we can test for that and retrieve the failure object using `insta/failure?` and `insta/get-failure`, respectively.

	=> (insta/failure? (repeated-a "aaaabaaa" :total true))
	true
	=> (insta/get-failure (repeated-a "aaaabaaa" :total true))
	Parse error at line 1, column 5:
	aaaabaaa
	    ^
	Expected:
	"a"

I find that the total parse mode is the most valuable diagnostic tool when the cause of the error is far away from the point where the parser actually fails.  A typical example might be a grammar where you are looking for phrases delimited by quotes, and the text neglects to include a closing quote mark around some phrase in the middle of the text.  The parser doesn't fail until it hits the end of the text without encountering a closing quote mark.

In such a case, a quick look at the total parse tree will show you the context of the failure, making it easy to spot the location where the run-on phrase began.

### Parsing from another start rule

Another valuable tool for interactive debugging is the ability to test out individual rules.  To demonstrate this, let's look back at our very first parser:

	=> as-and-bs
	S = AB*
	AB = A B
	A = "a"+
	B = "b"+

As we've seen throughout this tutorial, by default, instaparse assumes that the very first rule is your "starting production", the rule from which parsing initially proceeds.  But we can easily set other rules to be the starting production with the `:start` keyword argument.

	=> (as-and-bs "aaa" :start :A)
	[:A "a" "a" "a"]
	=> (as-and-bs "aab" :start :A)
	Parse error at line 1, column 3:
	aab
	  ^
	Expected:
	"a"
	=> (as-and-bs "aabb" :start :AB)
	[:AB [:A "a" "a"] [:B "b" "b"]]
	=> (as-and-bs "aabbaabb" :start :AB)
	Parse error at line 1, column 5:
	aabbaabb
	    ^
	Expected:
	"b"
	
The `insta/parser` function, which builds the parser from the specification, also accepts the :start keyword to set the default start rule to something other than the first rule listed.

#### Review of keyword arguments

At this point, you've seen all the keyword arguments that an instaparse-generated parser accepts, `:start :rule-name`, `:partial true`, and `:total true`. All these keyword arguments can be freely mixed and work with both `insta/parse` and `insta/parses`.

You've also seen both keyword arguments that can be used when building the parser from the specification: `:output-format (:enlive or :hiccup)` and `:start :rule-name` to set a different default start rule than the first rule.

### Transforming the tree

A parser's job is to turn a string into some kind of tree structure.  What you do with it from there is up to you.  It is delightfully easy to manipulate trees in Clojure.  There are wonderful tools available: enlive, zippers, match, and tree-seq.  But even without those tools, most tree manipulations are straightforward to perform in Clojure with recursion.

Since tree transformations are already so easy to perform in Clojure, there's not much point in building a sophisticated transform library into instaparse.  Nevertheless, I did include one function, `insta/transform`, that addresses the most common transformation needs.

`insta/transform` takes a map from tree tags to transform functions.  A transform function is defined as a function which takes the children of the tree node as inputs and returns a replacement node.  In other words, if you want to turn all nodes in your tree of the form `[:switch x y]` into `[:switch y x]`, then you'd call:

	(insta/transform {:switch (fn [x y] [:switch y x])}
		my-tree)

Let's make this concrete with an example.  So far, throughout the tutorial, we were able to adequately express the tokens of our languages with strings or regular expressions.  But sometimes, regular expressions are not sufficient, and we want to bring the full power of context-free grammars to bear on the problem of processing the individual tokens.  When we do that, we end up with a bunch of individual characters where we really want a string or a number.

To illustrate this, let's revisit the `words-and-numbers` example, but this time, we'll imagine that regular expressions aren't rich enough to specify the constraints on those tokens and we need our grammar to process the string one character at a time:

	(def words-and-numbers-one-character-at-a-time
	  (insta/parser
	    "sentence = token (<whitespace> token)*
	     <token> = word | number
	     whitespace = #'\\s+'
	     word = letter+
	     number = digit+
	     <letter> = #'[a-zA-Z]'
	     <digit> = #'[0-9]'"))

	=> (words-and-numbers-one-character-at-a-time "abc 123 def")
	[:sentence [:word "a" "b" "c"] [:number "1" "2" "3"] [:word "d" "e" "f"]]

We'd really like to simplify these `:word` and `:number` terminals.  So for `:word` nodes, we want to concatenate the strings with clojure's built-in `str` function, and for `:number` nodes, we want to concatenate the strings and convert the string to a number.  We can do this quite simply as follows:

	=> (insta/transform
	     {:word str,
	      :number (comp clojure.edn/read-string str)}
	     (words-and-numbers-one-character-at-a-time "abc 123 def"))
	[:sentence "abc" 123 "def"]

Or, if you're a fan of threading macros, try this version:

	=> (->> (words-and-numbers-one-character-at-a-time "abc 123 def")
	     (insta/transform
	       {:word str,
	        :number (comp clojure.edn/read-string str)}))

The `insta/transform` function auto-detects whether you are using enlive or hiccup trees, and processes accordingly.

`insta/transform` performs its transformations in a bottom-up manner, which means that taken to an extreme, `insta/transform` can be used not only to rearrange a tree, but to evaluate it.  Including a grammar for infix arithmetic math expressions has become nearly obligatory in parser tutorials, so I might as well use that in order to demonstrate evaluation.  I've leveraged instaparse's principle of "one rule per node type" and the hide notation `<>` to get a nice clean unambiguous tree that includes only the relevant information for evaluation.

	(def arithmetic
	  (insta/parser
	    "expr = add-sub
	     <add-sub> = mul-div | add | sub
	     add = add-sub <'+'> mul-div
	     sub = add-sub <'-'> mul-div
	     <mul-div> = term | mul | div
	     mul = mul-div <'*'> term
	     div = mul-div <'/'> term
	     <term> = number | <'('> add-sub <')'>
	     number = #'[0-9]+'"))

	=> (arithmetic "1-2/(3-4)+5*6")
	[:expr
	 [:add
	  [:sub
	   [:number "1"]
	   [:div [:number "2"] [:sub [:number "3"] [:number "4"]]]]
	  [:mul [:number "5"] [:number "6"]]]]

With the tree in this shape, it's trivial to evaluate it:

	=> (->> (arithmetic "1-2/(3-4)+5*6")
	     (insta/transform
	       {:add +, :sub -, :mul *, :div /,
	        :number clojure.edn/read-string :expr identity}))
	33

`insta/transform` is designed to play nicely with all the possible outputs of `insta/parse` and `insta/parses`.  So if the input is a sequence of parse trees, it will return a sequence of transformed parse trees.  If the input is a Failure object, then the Failure object is passed through unchanged.  This means you can safely chain a transform to your parser without taking special cases.  To demonstrate this, let's look back at the `ambiguous` parser from earlier in the tutorial:

	(def ambiguous
	  (insta/parser
	    "S = A A
	     A = 'a'*"))

	=> (->> (insta/parses ambiguous "aaaaaa")
	     (insta/transform {:A str}))
	([:S "a" "aaaaa"]
	 [:S "aaaaaa" ""]
	 [:S "aa" "aaaa"]
	 [:S "aaa" "aaa"]
	 [:S "aaaa" "aa"]
	 [:S "aaaaa" "a"]
	 [:S "" "aaaaaa"])

	 => (->> (ambiguous "aabaaa")
	     (insta/transform {:A str}))
	Parse error at line 1, column 3:
	aabaaa
	  ^
	Expected:
	"a"

### Understanding the tree

#### Character spans

The trees produced by instaparse are annotated with metadata so that for each subtree, you can easily recover the start and end index of the input text parsed by that subtree.  The convenience function for extracting this metadata is `insta/span`.  To demonstrate, let's revisit our first example.

	=> (as-and-bs "aaaaabbbaaaabb")
	[:S
	 [:AB [:A "a" "a" "a" "a" "a"] [:B "b" "b" "b"]]
	 [:AB [:A "a" "a" "a" "a"] [:B "b" "b"]]]

    => (meta (as-and-bs "aaaaabbbaaaabb"))
    {:instaparse.gll/start-index 0, :instaparse.gll/end-index 14}

	=> (insta/span (as-and-bs "aaaaabbbaaaabb"))
	[0 14]

	=> (count "aaaaabbbaaaabb")
	14

As you can see, `insta/span` returns a pair containing the start index (inclusive) and end index (exclusive), the customary way to represent the start and end of a substring.  So far, this isn't particularly interesting -- we already knew that the entire string was successfully parsed.   But since `span` works on all the subtrees, this gives us a powerful tool for exploring the provenance of each portion of the tree.  To demonstrate this, here's a quick helper function (not part of instaparse's API) that takes a hiccup tree and replaces all the tags with the character spans.

	(defn spans [t]
	  (if (sequential? t)
	    (cons (insta/span t) (map spans (next t)))
	    t))

	=> (spans (as-and-bs "aaaabbbaabbab"))
	([0 13]
	 ([0 7] ([0 4] "a" "a" "a" "a") ([4 7] "b" "b" "b"))
	 ([7 11] ([7 9] "a" "a") ([9 11] "b" "b"))
	 ([11 13] ([11 12] "a") ([12 13] "b")))

`insta/span` works on all the tree types produced by instaparse.  Furthermore, when you use `insta/transform` to transform your parse tree, `insta/span` will work on the transformed tree as well -- the span metadata is preserved for every node in the transformed tree to which metadata can be attached.  Keep in mind that although most types of Clojure data support metadata, primitives such as strings or numbers do not, so if you transform any of your nodes into such primitive data types, `insta/span` on those nodes will simply return `nil`.

##### Line and column information

Sometimes, when the input string contains newline characters, it is useful to have the span metadata in the form of line and column numbers.  By default, instaparse doesn't do this, because generating line and column information requires a second pass over the input string and parse tree.  However, the function `insta/add-line-and-column-info-to-metadata` performs this second pass, taking the input string and parse tree, returning a parse tree with the additional metadata.  Make sure to pass in the same input string from which the parse tree was derived!

    => (def multiline-text "This is line 1\nThis is line 2")

    => (words-and-numbers multiline-text)
    [:sentence [:word "This"] [:word "is"] [:word "line"] [:number "1"]
               [:word "This"] [:word "is"] [:word "line"] [:number "2"]]

    => (def parsed-multiline-text-with-line-and-column-metadata
         (insta/add-line-and-column-info-to-metadata
            multiline-text
            (words-and-numbers multiline-text)))
            
The additional information is in the metadata, so the tree itself is not visibly changed:

    => parsed-multiline-text-with-line-and-column-metadata
    [:sentence [:word "This"] [:word "is"] [:word "line"] [:number "1"] 
	           [:word "This"] [:word "is"] [:word "line"] [:number "2"]]

But now let's inspect the metadata for the overall parse tree.

    => (meta parsed-multiline-text-with-line-and-column-metadata)
    {:instaparse.gll/end-column 15, :instaparse.gll/end-line 2,
     :instaparse.gll/start-column 1, :instaparse.gll/start-line 1,
     :instaparse.gll/start-index 0, :instaparse.gll/end-index 29}

And let's take a look at the metadata for the word "is" on the second line of the text.

    => (meta (nth parsed-multiline-text-with-line-and-column-metadata 6))
    {:instaparse.gll/end-column 8, :instaparse.gll/end-line 2,
     :instaparse.gll/start-column 6, :instaparse.gll/start-line 2,
     :instaparse.gll/start-index 20, :instaparse.gll/end-index 22}]

start-line and start-column point to the same character as start-index, and end-line and end-column point to the same character as end-index.  So just like the regular span metadata, the line/column start point is inclusive and the end point is exclusive.  However, line and column numbers are 1-based counts, rather than 0-based.  So, for example, index number 0 of the string corresponds to line 1, column 1.

#### Visualizing the tree

Instaparse contains a function, `insta/visualize` *(Clojure only)*, that will give you a visual overview of the parse tree, showing the tags, the character spans, and the leaves of the tree.

	=> (insta/visualize (as-and-bs "aaabbab"))

<img src="images/vizexample1.png?raw=true" alt="Tree Image"/>

The visualize function, by default, pops open the tree in a new window.  To actually save the tree image as a file for this tutorial, I used both of the optional keyword arguments supported by `insta/visualize`.  First, the `:output-file` keyword argument supplies the destination where the image should be saved.  Second, the keyword `:options` is used to supply an option map of additional drawing parameters.  I lowered it to 63dpi so it wouldn't take up so much screen real estate.  So my function call looked like:

	=> (insta/visualize (as-and-bs "aaabbab") :output-file "images/vizexample1.png" :options {:dpi 63})

`insta/visualize` draws the tree using the [rhizome](https://github.com/ztellman/rhizome) library, which in turn uses [graphviz](http://www.graphviz.org).  Unfortunately, Java, and by extension Clojure, has a bit of a weakness when it comes to libraries depending on other libraries.  If you want to use two libraries that rely on two different versions of a third library, you're in for a headache.

In this instance, rhizome is a particularly fast-moving target.  As of the time of this writing, rhizome 0.1.8 is the most current version, released just a few weeks after version 0.1.6.  If I were to make instaparse depend on rhizome 0.1.8, then in a few weeks when 0.1.9 is released, it will become more difficult to use instaparse in projects which rely on the most recent version of rhizome.

For this reason, I've done something a bit unusual: rather than include rhizome directly in instaparse's dependencies, I've set things up so that `insta/visualize` will use whatever version of rhizome *you've* put in your project.clj dependencies (must be version 0.1.8 or greater).  On top of that, rhizome assumes that you have graphviz installed on your system.  If rhizome is not in your dependencies, or graphviz is not installed, `insta/visualize` will throw an error with a message reminding you of the necessary dependencies.  To find the most current version number for rhizome, and for links to graphviz installers, check out the [rhizome github site](https://github.com/ztellman/rhizome).

If you don't want to use `insta/visualize`, there is no need to add rhizome to your dependencies and no need to install graphviz.  All the other instaparse functions will work just fine.

### Combinators

I truly believe that ordinary EBNF notation is the clearest, most concise way to express a context-free grammar.  Nevertheless, there may be times when it is useful to build parsers with parser combinators.  If you want to use instaparse in this way, you'll need to use the `instaparse.combinators` namespace.  If you are not interested in the combinator interface, feel free to skip this section -- the combinators provide no additional power or expressiveness over the string representation.

Each construct you've seen from the string specification has a corresponding parser combinator.  Most are straightforward, but the last few lines of the table will require some additional explanation.

<table>
<tr><th>String syntax</th><th>Combinator</th><th>Mnemonic</th></tr>
<tr><td>Epsilon</td><td>Epsilon</td><td>Epsilon</td></tr>
<tr><td>A | B | C</td><td>(alt A B C)</td><td>Alternation</td></tr>
<tr><td>A B C</td><td>(cat A B C)</td><td>Concatenation</td></tr>
<tr><td>A?</td><td>(opt A)</td><td>Optional</td></tr>
<tr><td>A+</td><td>(plus A)</td><td>Plus</td></tr>
<tr><td>A*</td><td>(star A)</td><td>Star</td></tr>
<tr><td>A / B / C</td><td>(ord A B C)</td><td>Ordered Choice</td></tr>
<tr><td>&A</td><td>(look A)</td><td>Lookahead</td></tr>
<tr><td>!A</td><td>(neg A)</td><td>Negative lookahead</td></tr>
<tr><td>&lt;A&gt;</td><td>(hide A)</td><td>Hide</td></tr>
<tr><td>"string"</td><td>(string "string")</td><td>String</td></tr>
<tr><td>#"regexp"</td><td>(regexp "regexp")</td><td>Regular Expression</td></tr>
<tr><td>A non-terminal</td><td>(nt :non-terminal)</td><td>Non-terminal</td></tr>
<tr><td>&lt;S&gt; = ...</td><td>{:S (hide-tag ...)}</td><td>Hide tag</td></tr>
</table>

When using combinators, instead of building a string, your goal is to build a *grammar map*.  So a spec that looks like this:

	S = ...
	A = ...
	B = ...

becomes

	{:S ... combinators describing right-hand-side of S rule ...
	 :A ... combinators describing right-hand-side of A rule ...
	 :B ... combinators describing right-hand-side of B rule ...}

You can also build it as a vector:

	[:S ... combinators describing right-hand-side of S rule ...
	 :A ... combinators describing right-hand-side of A rule ...
	 :B ... combinators describing right-hand-side of B rule ...]

The main difference is that if you use the map representation, you'll eventually need to specify the start rule, but if you use the vector, instaparse will assume the first rule is the start rule.  Either way, I'm going to refer to the above structure as a *grammar map*.

Most of the combinators, if you consult the above table, are pretty obvious.  Here are a few additional things to keep in mind, and then a concrete example will follow:

1. Literal strings must be wrapped in a call to the `string` combinator.

2. Regular expressions must be wrapped in a call to the `regexp` combinator.

3. Any reference on the right-hand side of a rule to a non-terminal (i.e., a name of another rule) must be wrapped in a call to the `nt` combinator.

4. Angle brackets on the right-hand side of a rule correspond to the `hide` combinator.

5. Even though the notation for hiding a rule name is to put angle brackets around the name (on the left-hand side), this is implemented by wrapping the `hide-tag` combinator around the entire *right-hand side* of the rule expressed as combinators.

Hopefully this will all be clarified with an example.  Do you remember the parser that looks for equal numbers of a's followed by b's followed by c's?

	S = &(A 'c') 'a'+ B
    A = 'a' A? 'b'
    <B> = 'b' B? 'c'

Well, here's the corresponding grammar map:

	(use 'instaparse.combinators)

	(def abc-grammar-map
	  {:S (cat (look (cat (nt :A) (string "c")))
	           (plus (string "a"))
	           (nt :B))
	   :A (cat (string "a") (opt (nt :A)) (string "b"))
	   :B (hide-tag (cat (string "b") (opt (nt :B)) (string "c")))})

Once you've built your grammar map, you turn it into an executable parser by calling `insta/parser`.  As I mentioned before, if you use map notation, you'll need to specify the start rule.

	(insta/parser abc-grammar-map :start :S)

The result is a parser that is the same as the one built from the string specification.

To my eye, the string is dramatically more readable, but if you need or want to use the combinator approach, it's there for you to utilize.

#### String to combinator conversion

Shortly after I published the first version of instaparse, I received a question, "String specifications can be combined with `clojure.string/join` and combinator grammar maps can be combined with `merge` --- is there any way to mix and match string and combinator grammar representations?"  At the time, there wasn't, but now there is.  As of version 1.1, there is a new function `ebnf` in the `instaparse.combinators` namespace which *converts* EBNF strings into the same underlying structure that is built by the combinator library, thus allowing for further manipulation by combinators.  (EBNF stands for Extended Backus-Naur Form, the technical name for the syntax used by instaparse and described in this tutorial.)  For example,

	(ebnf "'a'* | 'b'+")

produces the same structure as if you had typed the combinator version

	(alt (star (string "a")) (plus (string "b")))

You can also pass entire rules to `ebnf` and you'll get back the corresponding grammar map:

	(ebnf "A = 'a'*; B = 'b'+")

produces

	{:A (star (string "a"))
	 :B (plus (string "b"))}

This opens up the possibility of building a grammar from a mixture of combinators, and strings that have been converted to combinators.  Here's a contrived example:

	(def combo-build-example
	  (insta/parser
	    (merge
	      {:S (alt (nt :A) (nt :B))}
	      (ebnf "A = 'a'*")
	      {:B (ebnf "'b'+")})
	    :start :S))

### ABNF

Instaparse's primary input format is based on EBNF syntax, but an alternative input format, ABNF, is available.  Most users will not need the ABNF input format, but if you need to implement a parser whose specification was written in ABNF syntax, it is very easy to do.  Please read [instaparse's ABNF documentation](https://github.com/Engelberg/instaparse/blob/master/docs/ABNF.md) for details.

### String case sensitivity

One notable difference between EBNF and ABNF notations is that in EBNF, string terminals are case-sensitive, whereas in ABNF, string terminals are traditionally case-*in*sensitive.

Instaparse follows the respective behaviors of the two notations by default, but in both cases, the case-sensitivity is overridable with the `:string-ci` flag, which states for sure whether all string terminals in the grammar are case-insensitive.

```
;; EBNF, case-sensitive
=> ((insta/parser "S = 'a'+") "AaaAaa")
Parse error at line 1, column 1:
AaaAaa
^
Expected:
"a"

;; EBNF, case-insensitive (override)
=> ((insta/parser "S = 'a'+" :string-ci true) "AaaAaa")
[:S "a" "a" "a" "a" "a" "a"]

;; ABNF, case-insensitive
=> ((insta/parser "S = 1*'a'" :input-format :abnf) "AaaAaa")
[:S "a" "a" "a" "a" "a" "a"]

;; ABNF, case-sensitive (override)
=> ((insta/parser "S = 1*'a'" :input-format :abnf :string-ci false) "AaaAaa")
Parse error at line 1, column 1:
AaaAaa
^
Expected:
"a"
```

On the other hand, if you want to mix and match case-sensitive and case-insensitive strings within a grammar, you can convert strings to regexes, such as `#'I am case-sensitive'` or `#'(?i)I am case-insensitive'`. The `ebnf` and `abnf` combinators also support the `:string-ci` flag.

### Serialization

You can serialize an instaparse parser with `print-dup`, and deserialize it with `read`.  (You can't use `clojure.edn/read` because edn does not support regular expressions.)

Typically, it is more convenient to store and/or transmit the string specification used to generate the parser.  The string specification allows the parser to be rebuilt with a different output format; `print-dup` captures the state of the parser after the output format has been "baked in".  However, if you have built the parser with the combinators, rather than via a string spec, or if you are storing the parser inside of other Clojure data structures that need to be serialized, then `print-dup` may be your best option.

## Performance notes

Some of the parsing libraries out there were written as a learning exercise -- monadic parser combinators, for example, are a great way to develop an appreciation for monads.  There's nothing wrong with taking the fruits of a learning exercise and making it available to the public, but there are enough Clojure parser libraries out there that it is getting to be hard to tell the difference between those that are "ready for primetime" and those that aren't.  For example, some of the libraries rely heavily on nested continuations, a strategy that is almost certain to cause a stack overflow on moderately large inputs.  Others rely heavily on memoization, but never bother to clear the cache between inputs, eventually exhausting all available memory if you use the parser repeatedly.

I'm not going to make any precise performance guarantees -- the flexible, general nature of instaparse means that it is possible to write grammars that behave poorly.  Nevertheless, I want to convey that performance is something I have taken seriously.  I spent countless hours profiling instaparse's behavior on strange grammars and large inputs, using that data to improve performance.  Just as one example, I discovered that for a large class of grammars, the biggest bottleneck was Clojure's hashing strategy, so I implemented a wrapper around Clojure's vectors that uses an alternative hashing strategy, successfully reducing running time on many parsers from quadratic to linear.  (A shout-out to Christophe Grand who provided me with valuable guidance on this particular improvement.)

I've also worked to remove "performance surprises".  For example, both left-recursion and right-recursion have sufficiently similar performance that you really don't need to agonize over which one to use -- choose whichever style best fits the problem at hand.  If you express your grammar in a natural way, odds are good that you'll find the performance of the generated parser to be satisfactory.  An additional performance boost in the form of multithreading is slated for the next release.

One performance caveat: instaparse is fairly memory-hungry, relying on extensive caching of intermediate results to keep the computational costs reasonable.  This is not unusual -- caching is commonplace in many modern parsers, trading off space for time -- but it's worth bearing in mind.  Packrat/PEG parsers and many recursive descent parsers employ a similar memory-intensive strategy, but there are other alternatives out there if that kind of memory usage is unacceptable.  As one would expect, instaparse parsers do not hold onto the memory cache once the parse is complete; that memory is made available for garbage collection.

The [performance notes document](https://github.com/Engelberg/instaparse/blob/master/docs/Performance.md) contains a deeper discussion of performance and a few helpful hints for getting the best performance out of your parser.

## Reference

All the functionality you've seen in this tutorial is packed into an API of just 9 functions.  Here are the doc strings:

	=> (doc insta/parser)
	-------------------------
	instaparse.core/parser
	([grammar-specification & {:as options}])
	  Takes a string specification of a context-free grammar,
	   or a URI for a text file containing such a specification,
	   or a map of parser combinators and returns a parser for that grammar.

	   Optional keyword arguments:
	   :input-format :ebnf
	   or
	   :input-format :abnf

	   :output-format :enlive
	   or
	   :output-format :hiccup

	   :start :keyword (where :keyword is name of starting production rule)

	   :string-ci true (treat all string literals as case insensitive)

	   :auto-whitespace (:standard or :comma)
	   or
	   :auto-whitespace custom-whitespace-parser

       Clj only:
	   :no-slurp true (disables use of slurp to auto-detect whether
	                   input is a URI.  When using this option, input
	                   must be a grammar string or grammar map.  Useful
	                   for platforms where slurp is slow or not available.)

	=> (doc insta/parse)
	-------------------------
	instaparse.core/parse
	([parser text & {:as options}])
	  Use parser to parse the text.  Returns first parse tree found
	   that completely parses the text.  If no parse tree is possible, returns
	   a Failure object.

	   Optional keyword arguments:
	   :start :keyword  (where :keyword is name of starting production rule)
	   :partial true    (parses that don't consume the whole string are okay)
	   :total true      (if parse fails, embed failure node in tree)
	   :unhide <:tags or :content or :all> (for this parse, disable hiding)
	   :optimize :memory   (when possible, employ strategy to use less memory)

       Clj only:
       :trace true      (print diagnostic trace while parsing)

	=> (doc insta/parses)
	-------------------------
	instaparse.core/parses
	([parser text & {:as options}])
	  Use parser to parse the text.  Returns lazy seq of all parse trees
	   that completely parse the text.  If no parse tree is possible, returns
	   () with a Failure object attached as metadata.

	   Optional keyword arguments:
	   :start :keyword  (where :keyword is name of starting production rule)
	   :partial true    (parses that don't consume the whole string are okay)
	   :total true      (if parse fails, embed failure node in tree)
	   :unhide <:tags or :content or :all> (for this parse, disable hiding)

       Clj only:
       :trace true      (print diagnostic trace while parsing)

    => (doc insta/set-default-output-format!)
	-------------------------
	instaparse.core/set-default-output-format!
	([type])
	  Changes the default output format.  Input should be :hiccup or :enlive

	=> (doc insta/failure?)
	-------------------------
	instaparse.core/failure?
	([result])
	  Tests whether a parse result is a failure.

	=> (doc insta/get-failure)
	-------------------------
	instaparse.core/get-failure
	([result])
	  Extracts failure object from failed parse result.

	=> (doc insta/transform)
	-------------------------
	instaparse.core/transform
	([transform-map parse-tree])
	  Takes a transform map and a parse tree (or seq of parse-trees).
	   A transform map is a mapping from tags to
	   functions that take a node's contents and return
	   a replacement for the node, i.e.,
	   {:node-tag (fn [child1 child2 ...] node-replacement),
	    :another-node-tag (fn [child1 child2 ...] node-replacement)}

	=> (doc insta/span)
	-------------------------
	instaparse.core/span
	([tree])
	  Takes a subtree of the parse tree and returns a [start-index end-index] pair
	   indicating the span of text parsed by this subtree.
	   start-index is inclusive and end-index is exclusive, as is customary
	   with substrings.
	   Returns nil if no span metadata is attached.

    => (doc insta/add-line-and-column-info-to-metadata)
    -------------------------
    instaparse.core/add-line-and-column-info-to-metadata
    ([text parse-tree])
      Given a string `text` and a `parse-tree` for text, return parse tree
       with its metadata annotated with line and column info. The info can
       then be found in the metadata map under the keywords:

      :instaparse.gll/start-line, :instaparse.gll/start-column,
      :instaparse.gll/end-line, :instaparse.gll/end-column

      The start is inclusive, the end is exclusive. Lines and columns are 1-based.

	=> (doc insta/visualize)
	-------------------------
	instaparse.core/visualize
	([tree & {output-file :output-file, options :options}])
	  Creates a graphviz visualization of the parse tree.
	   Optional keyword arguments:
	   :output-file output-file (will save the tree image to output-file)
	   :options options (options passed along to rhizome)

	Important: This function will only work if you have added rhizome
	to your dependencies, and installed graphviz on your system.
	See https://github.com/ztellman/rhizome for more information.

## Experimental Features

See the [Experimental Features](docs/ExperimentalFeatures.md) page for a discussion of new features under active development, including memory optimization and automatic handling of whitespace.

## Communication

I try to be very responsive to issues posted to the github issues page.  But if you have a general question, need some help troubleshooting a grammar, or have something interesting you've done in instaparse that you'd like to share, consider joining the [Instaparse Google Group](https://groups.google.com/d/forum/instaparse) and posting there.

## Special Thanks

My interest in this project began while watching a video of Matt Might's [*Parsing with Derivatives*](http://www.youtube.com/watch?v=ZzsK8Am6dKU) talk.  That video convinced me that the world would be a better place if building parsers were as easy as working with regular expressions, and that the ability to handle arbitrary, possibly-ambiguous grammars was essential to that goal.

Matt Might has published a [paper](http://matt.might.net/papers/might2011derivatives.pdf) about a specific approach to achieving that goal, but I had difficulty getting his *Parsing with Derivatives* technique to work in a performant way.

I probably would have given up, but then Danny Yoo released the [Ragg parser generator](http://hashcollision.org/ragg/index.html) for the Racket language.  The Ragg library was a huge inspiration -- a model for what I wanted instaparse to become.  I asked Danny what technique he used, and he gave me more information about the algorithm he used.  However, he told me that if he were to do it again from scratch, he'd probably choose to use a [GLL algorithm](http://ldta.info/2009/ldta2009proceedings.pdf) by Adrian Johnstone and Elizabeth Scott, and he pointed me to a fantastic article about it by Vegard Øye, [posted on Github with source code in Racket](https://github.com/epsil/gll).

That article had a link to a [paper](http://www.cs.uwm.edu/%7Edspiewak/papers/generalized-parser-combinators.pdf) and [Scala code](https://github.com/djspiewak/gll-combinators) by Daniel Spiewak, which was also extremely helpful.

Alex Engelberg coded the first version of instaparse, proving the capabilities of the GLL algorithm.  He encouraged me to take his code and build and document a user-friendly API around it.  He continues to be a main contributor on the project, most recently developing the ABNF front-end, bringing the Clojurescript port up to feature parity with the Clojure version, and working out the details of merging the two codebases.

I studied a number of other Clojure parser generators to help frame my ideas about what the API should look like.  I communicated with Eric Normand ([squarepeg](https://github.com/ericnormand/squarepeg)) and Christophe Grand ([parsley](https://github.com/cgrand/parsley)), both of whom provided useful advice and encouraged me to pursue my vision.

YourKit is kindly supporting open source projects with its full-featured Java Profiler.
YourKit, LLC is the creator of innovative and intelligent tools for profiling
Java and .NET applications. Take a look at YourKit's leading software products:
[YourKit Java Profiler](http://www.yourkit.com/java/profiler/index.jsp) and
[YourKit .NET Profiler](http://www.yourkit.com/.net/profiler/index.jsp).

## License

Distributed under the Eclipse Public License, the same as Clojure.

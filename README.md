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

### Alternative notations

Instaparse supports most of the common notations for context-free grammars.  For example, a popular alternative to `*` is to surround the term with curly braces `{}`, and a popular alternative to `?` is to surround the term with square brackets `[]`.  Rules can be specified with `=`, `:`, `:=`, or `::=`.  Rules can optionally end with `;`.  Instaparse is very flexible in terms of how you use whitespace (as in Clojure, `,` is treated as whitespace) and you can liberally use parentheses for grouping.  Terminal strings can be enclosed in either single quotes or double quotes (however, since you are writing the grammar specification inside of a Clojure double-quoted strings, any uses of double-quotes would have to be escaped, therefore single-quotes are easier to read). Newlines are optional; you can put the entire grammar on one line if you desire.  In fact, all these notations can be mixed up in the same specification if you want.

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

A table of the supported syntaxes

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

**** Hiding content

For this next example, let's consider a parser that looks for a sequence of a's or b's surrounded by parens.

	(def paren-ab
	  (insta/parser
	    "paren-wrapped = '(' seq-of-A-or-B ')'
	     seq-of-A-or-B = ('a' | 'b')*"))
	
	=> (paren-ab "(aba)")
	[:paren-wrapped "(" [:seq-of-A-or-B "a" "b" "a"] ")"]

It's very common in parsers to have elements that need to be present in the input and parsed, but we'd rather not have them appear in the output.    In the above example, the parens are essential to the grammar yet the tree would be much easier to read and manipulate if we could hide those parens; once the string has been parsed, the parens themselves carry no additional semantic value.

In Instaparse, you can use angle brackets `<>` to hide parsed elements, suppressing them from the tree output.

	(def paren-ab-hide-parens
	  (insta/parser
	    "paren-wrapped = <'('> seq-of-A-or-B <')'>
	     seq-of-A-or-B = ('a' | 'b')*"))

	=> (paren-ab-hide-parens "(aba)")
	[:paren-wrapped [:seq-of-A-or-B "a" "b" "a"]]

Voila! The parens "(" and ")" tokens have been hidden.  Angle brackets are a powerful tool for hiding whitespace and other delimiters from the output.

**** Hiding tags
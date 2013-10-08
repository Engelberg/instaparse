# Instaparse Experimental Features

This document provides an explanation of some of the things I'm experimenting with in instaparse.  Please try the new features and let me know what you think.

## Optimizing memory

I've added a new, experimental `:optimize :memory` flag that can conserve memory usage for certain classes of grammars.  I discussed the motivation for this in the [Performance document](Performance.md).  The idea is to make it more practical to use instaparse in situations where you need to parse files containing a large number of independent chunks.

Usage looks like this:

	(def my-parser (insta/parser my-grammar))
	(my-parser text :optimize :memory)


It works for grammars where the top-level production is of the form

	start = chunk+

or

	start = header chunk+

I don't mean that it literally needs to use the words `start` or `header` or `chunk`.  What I mean is that the optimizer looks for top-level productions that finish off with some sort of repeating structure.  To be properly optimized, you want to ensure that the `chunk` rule is written with no ambiguity about where a chunk begins and ends.

Behind the scenes, here's what the optimization algorithm is doing:  After successfully parsing a `chunk`, the parser *forgets* all the backtracking information and continues parsing the remaining text totally fresh looking for the next chunk, with no sense of history about what has come before.  As long as it keeps finding one chunk after another, it can get through a very large file with far less memory usage than the standard algorithm.

The downside of this approach is that if the parser hits a spot that doesn't match the repeating chunk rule, there's no way for it to know for sure that this is a fatal failure.  It is entirely possible that there is some other interpretation of an eariler chunk that would make the whole input parseable.  The standard instaparse approach is to backtrack and look for alternative interpretations before declaring a failure.  However, without that backtracking history, there's no way to do that.

So when you use the `:optimize :memory` flag and your parser hits an error using the "parse one chunk at a time and forget the past" strategy, it *restarts the entire parse process* with the original strategy.

I'm not entirely sure this was the right design decision, and would welcome feedback on this point.  Here are the tradeoffs:

Advantage of the current approach:  With this *fall back to the original strategy if the optimizer doesn't work* approach, it should be totally safe to try the optimizer, even if you don't know for sure up front whether the optimizer will work.  With the `:optimize :memory` flag, the output will always be exactly the same as if you hadn't used the flag.  (A metadata annotation, however, will let you know whether the parse was successfully completed entirely with the optimization strategy.)  I like the safety of this approach, and how it is amenable to the attitude of "Let's try this optimization flag out and see if it helps."

Disadvantage of the current approach: If you're operating on a block of input text so large that the memory optimization is a *necessity*, then if you have a flaw in your text, you're in trouble -- the parsing restarts with the original strategy and if the flaw is fairly late in your file, you could exhaust your memory.

An alternative design would be to say that if you've enabled the `:optimize :memory` flag, and it hits an apparent flaw in the input, then it's immediately reported as a failure, without any attempt to try the more sophisticated strategy and see whether backtracking might help the situation.  This would be good for people willing to expend the effort to ensure the grammar conforms to the optimizer's constraints and has no ambiguity in the chunk definition.  It would then correct to report a failure right away if encountered by the optimization strategy -- no need to fall back to the original strategy because there's no ambiguity and no alternative interpretation.

However, if the flag behaved in this way, then it is possible that if the grammar weren't well-suited for the optimizer, the `:optimize :memory` flag might return a failure in some instances where the regular strategy would return success.  In some sense, this would give the programmer maximum control: the programmer can *choose* to rerun the input without the `:optimize :memory` flag or can accept the failure at face value if confident in the grammar's suitability for the optimization strategy.

So I'm torn: right now the optimizer falls back to the regular strategy because I like that it is dead simple to use, it's safe to try without a deep understanding of what is going on, and it will always give correct output.  But I recognize that having the optimizer simply report the failure gives the programmer greatest control over whether to restart with the regular strategy or not.

What do you think is the better design choice?

## Auto Whitespace

I have received several requests for instaparse to support the parsing of streams of tokens, rather than just strings.  There appear to be two main motivations for this request:

1. For some grammars, explicitly specifying all the places where whitespace can go is a pain.
2. For parsing indentation-sensitive languages, it is useful to have a pre-processing pass that identifies `indent` and `dedent` tokens.

I'm still thinking about developing a token-processing version of instaparse.  But if I can find a way to address the underlying needs while maintaining the "token-free" simplicity of instaparse, that would be even better.

This new experimental "auto whitespace" feature addresses the first issue, simplifying the specification of grammars where you pretty much want to allow optional whitespace between all your tokens.  Here's how to use the new feature:

First, you want to develop a parser that consumes whitespace.  The simplest, most common way to do this would be:

	(def whitespace
	  (insta/parser
	    "whitespace = #'\\s+'"))
	    
Let's test it out:

	=> (whitespace "       ")
	[:whitespace "       "]
	=> (whitespace " \t \n  \t ")
	[:whitespace " \t \n  \t "]
	
Important: Your whitespace parser should *not* accept the empty string.

	=> (whitespace "")
	Parse error at line 1, column 1:
	nil
	^
	Expected:
	#"^\s+" (followed by end-of-string)
	
Good, this is what we want.  Now, we can define a parser similar to the `words-and-numbers` parser from the tutorial, but this time we'll use the auto-whitespace feature.

	(def words-and-numbers-auto-whitespace
	  (insta/parser
	    "sentence = token+
	     <token> = word | number
	     word = #'[a-zA-Z]+'
	     number = #'[0-9]+'"

	    :auto-whitespace whitespace))

Notice the use of the `:auto-whitespace` keyword, and how we call it with the whitespace parser we developed earlier.

	=> (words-and-numbers-auto-whitespace " abc 123   45 de ")
	[:sentence [:word "abc"] [:number "123"] [:number "45"] [:word "de"]]

Behind the scenes, here's what's going on: the whitespace parsing rule(s) are merged into the new parser, and an optional version of the starting production for the whitespace rule is liberally inserted before all tokens and at the end.  In this case, that means `<whitespace?>` is inserted all over the place.  You can see the insertion points by viewing the parser:

	=> words-and-numbers-auto-whitespace

	sentence = token+ whitespace?
	whitespace = #"\s+"
	token = word | number
	word = whitespace? #"[a-zA-Z]+"
	number = whitespace? #"[0-9]+"

You can also see that the whitespace is in fact getting parsed, and is just being hidden:

	=> (words-and-numbers-auto-whitespace " abc 123   45 de " :unhide :content)
	[:sentence " " [:word "abc"] " " [:number "123"] "   " [:number "45"] " " [:word "de"] " "]

Because the whitespace parser rules are merged into the new parser, don't create any rules in your parser with the same names as those in the whitespace parser.  If you do, one of the rules will get clobbered and you'll run into problems.  (TODO: Report an error if a user tries to do this)

Note that it makes no difference whether the `:output-format` of the whitespace parser is :enlive or :hiccup.  The rules and the starting production for the whitespace parser are all that matter.

Because the :auto-whitespace feature allows you to specify your notion of whitespace, you have the total flexibility to define this however you want.  For example, let's say I want to allow not only whitespace, but `(* comments *)` between any tokens.  Again, we start by developing a corresponding parser:

	(def whitespace-or-comments-v1
	  (insta/parser
	    "ws-or-comment = #'\\s+' | comment
	     comment = '(*' inside-comment* '*)'
	     inside-comment =  ( !('*)' | '(*') #'.' ) | comment"))

Does it eat whitespace?

	=> (whitespace-or-comments-v1 "    ")
	[:ws-or-comment "    "]

Check.  Does it handle a comment?

	=> (whitespace-or-comments-v1 "(* comment *)")
	<successful parse output omitted>

Check.  Can it handle nested comments?

	=> (whitespace-or-comments-v1 "(* (* comment *) *)")
	<successful parse output omitted>

And we mustn't forget -- make sure it *doesn't* parse the empty string:

	=> (whitespace-or-comments-v1 "")
	<failure message omitted>

However, there's a problem here.  The auto-whitespace feature inserts optional `?` versions of the whitespace parser everywhere, *not* repeating versions.  It's up to us to make sure that the whitespace parser consumes the *full extent* of any whitespace that could appear between tokens.  In other words, if we want to allow multiple comments in a row, we need to spell that out:

	(def whitespace-or-comments-v2
	  (insta/parser
	    "ws-or-comments = #'\\s+' | comments
	     comments = comment+
	     comment = '(*' inside-comment* '*)'
	     inside-comment =  ( !('*)' | '(*') #'.' ) | comment"))

	=> (whitespace-or-comments-v2 "(* comment1 *)(* (* nested comment *) *)")
	<successful parse output omitted>

There's still one more issue, though.  Right now, our parser specifies complete empty whitespace, or a series of comments.  But if we want to intermingle whitespace and comments, it won't work:

	=> (whitespace-or-comments-v2 "  (* comment1 *) (* comment2 *) ")
	Parse error at line 1, column 1:
	  (* comment1 *) (* comment2 *)
	^
	Expected one of:
	#"^\s+" (followed by end-of-string)
	"(*"

I could go through and manually insert optional whitespace, but wouldn't it be deliciously meta to use the auto-whitespace feature with our previous, simple whitespace parser to define our whitespace-or-comments parser?

	(def whitespace-or-comments
	  (insta/parser
	    "ws-or-comments = #'\\s+' | comments
	     comments = comment+
	     comment = '(*' inside-comment* '*)'
	     inside-comment =  ( !('*)' | '(*') #'.' ) | comment"

    	:auto-whitespace whitespace))

Now it works:

	=> (whitespace-or-comments "  (* comment1 *) (* comment2 *) ")
	<successful parse output omitted>

Just out of curiosity, let's see where the `<whitespace?>` got inserted:

	=> whitespace-or-comments
	ws-or-comments = (whitespace? #"\s+" | comments) whitespace?
	whitespace = #"\s+"
	comments = comment+
	comment = whitespace? "(*" inside-comment* whitespace? "*)"
	inside-comment = !(whitespace? "*)" | whitespace? "(*") whitespace? #"." | comment

Note that the auto-insertion process inserted `whitespace?` right before the `"*)"`, but this isn't particularly useful, because all whitespace before `*)` would already be eaten by the `inside-comment` rule.  If you were inserting the optional whitespace by hand, you'd probably realize it was unnecessary there.  However, when you let the system automatically insert it everywhere, some of the insertions might be gratuitous.  But that's okay, having the extra optional whitespace inserted there doesn't really hurt us either.

Now that we have thoroughly tested our whitespace-or-comments parser, we can use it to enrich our words-and-numbers parser:

	(def words-and-numbers-auto-whitespace-and-comments
	  (insta/parser
	    "sentence = token+
	     <token> = word | number
	     word = #'[a-zA-Z]+'
	     number = #'[0-9]+'"

	    :auto-whitespace whitespace-or-comments))

	=> (words-and-numbers-auto-whitespace-and-comments " abc 123 (* 456 *) (* (* 7*) 89 *)  def ")
	[:sentence [:word "abc"] [:number "123"] [:word "def"]]

	=> words-and-numbers-auto-whitespace-and-comments

	sentence = token+ ws-or-comments?
	inside-comment = !(whitespace? "*)" | whitespace? "(*") whitespace? #"." | comment
	comment = whitespace? "(*" inside-comment* whitespace? "*)"
	comments = comment+
	ws-or-comments = (whitespace? #"\s+" | comments) whitespace?
	whitespace = #"\s+"
	token = word | number
	word = ws-or-comments? #"[a-zA-Z]+"
	number = ws-or-comments? #"[0-9]+"

Note that this feature is only useful in grammars where all the strings and regexes are, conceptually, the "tokens" of your language.  Occasionally, you'll see situations where grammars specify tokens through rules that build up the tokens character-by-character, for example:

    month = ('M'|'m') 'arch'
    
If you try to use the auto-whitespace feature with a grammar like this, it will end up allowing space between the "m" and the "arch", which isn't what you want.  The key is to try to express such tokens using a single regular expression:

	month = #'[Mm]arch'

Let me know what you think of the auto-whitespace feature.  Is it sufficiently simple and useful to belong in the instaparse library?
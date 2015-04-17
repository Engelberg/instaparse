# Tracing

Instaparse 1.4.0 and up features the ability to look at a trace of what the parser is doing.  As an example, let's take a look at the as-and-bs parser from the tutorial.

```
=> (as-and-bs "aaabb")
[:S [:AB [:A "a" "a" "a"] [:B "b" "b"]]]
```

Now let's look at a trace.  We do this by calling the parser with the optional keyword argument `:trace true`.  `insta/parse` and `insta/parses` both can take this optional argument.

```
=> (as-and-bs "aaabb" :trace true)
```

One of my design goals for the tracing feature was that if you don't use it, you shouldn't pay a performance penalty.  So by default, the parsing code is not instrumented for tracing.  The very first time you call a parser with `:trace true`, you may notice a slight pause as instaparse recompiles itself to support tracing.  The trace the prints to standard out, and looks like this:

```
Initiating full parse: S at index 0 (aaabb)
Initiating full parse: AB* at index 0 (aaabb)
Initiating parse: AB at index 0 (aaabb)
Initiating parse: A B at index 0 (aaabb)
Initiating parse: A at index 0 (aaabb)
Initiating parse: "a"+ at index 0 (aaabb)
Initiating parse: "a" at index 0 (aaabb)
Result for "a" at index 0 (aaabb) => "a"
Result for "a"+ at index 0 (aaabb) => ("a")
Result for A at index 0 (aaabb) => [:A "a"]
Initiating parse: B at index 1 (aabb)
Initiating parse: "b"+ at index 1 (aabb)
Initiating parse: "b" at index 1 (aabb)
No result for "b" at index 1 (aabb)
Initiating parse: "a" at index 1 (aabb)
Result for "a" at index 1 (aabb) => "a"
Result for "a"+ at index 0 (aaabb) => ("a" "a")
Result for A at index 0 (aaabb) => [:A "a" "a"]
Initiating parse: B at index 2 (abb)
Initiating parse: "b"+ at index 2 (abb)
Initiating parse: "b" at index 2 (abb)
No result for "b" at index 2 (abb)
Initiating parse: "a" at index 2 (abb)
Result for "a" at index 2 (abb) => "a"
Result for "a"+ at index 0 (aaabb) => ("a" "a" "a")
Result for A at index 0 (aaabb) => [:A "a" "a" "a"]
Initiating parse: B at index 3 (bb)
Initiating parse: "b"+ at index 3 (bb)
Initiating parse: "b" at index 3 (bb)
Result for "b" at index 3 (bb) => "b"
Result for "b"+ at index 3 (bb) => ("b")
Result for B at index 3 (bb) => [:B "b"]
Result for A B at index 0 (aaabb) => ([:A "a" "a" "a"] [:B "b"])
Result for AB at index 0 (aaabb) => [:AB [:A "a" "a" "a"] [:B "b"]]
Initiating parse: AB at index 4 (b)
Initiating parse: A B at index 4 (b)
Initiating parse: A at index 4 (b)
Initiating parse: "a"+ at index 4 (b)
Initiating parse: "a" at index 4 (b)
No result for "a" at index 4 (b)
Initiating parse: "b" at index 4 (b)
Result for "b" at index 4 (b) => "b"
Result for "b"+ at index 3 (bb) => ("b" "b")
Result for B at index 3 (bb) => [:B "b" "b"]
Result for A B at index 0 (aaabb) => ([:A "a" "a" "a"] [:B "b" "b"])
Result for AB at index 0 (aaabb) => [:AB [:A "a" "a" "a"] [:B "b" "b"]]
Result for AB* at index 0 (aaabb) => ([:AB [:A "a" "a" "a"] [:B "b" "b"]])
Result for S at index 0 (aaabb) => [:S [:AB [:A "a" "a" "a"] [:B "b" "b"]]]
Successful parse.
Profile:  {:push-message 21, :push-result 21, :push-listener 24, :push-stack 26, :push-full-listener 2, :create-node 26}
[:S [:AB [:A "a" "a" "a"] [:B "b" "b"]]]
```

Let me explain what some of these lines mean.

```
Initiating full parse: S at index 0 (aaabb)
```

A "full parse" means that it only succeeds if it consumes the entire string.  Usually, we're looking to completely parse an entire string, and that's what "full parse" reflects.

It is important to understand that the word "intiating" does not necessarily mean that it is starting to work on that parse sub-problem right away.  It just means that we're putting it on a stack of sub-problems to try to solve.

Notice the `(aaabb)` in parens.  This is giving us the next several characters from this point in the string, which makes it a little easier to see at a glance where we are in the string (although, of course the index number can always be used to figure it out precisely).

```
Initiating full parse: AB* at index 0 (aaabb)
Initiating parse: AB at index 0 (aaabb)
```

Note that AB* needs to be a full parse to be satisfied, but that kicks off another subproblem, which is to look for a parse of AB (not necessarily a full parse) at index 0.

```
Initiating parse: A at index 0 (aaabb)
Initiating parse: "a"+ at index 0 (aaabb)
Initiating parse: "a" at index 0 (aaabb)
Result for "a" at index 0 (aaabb) => "a"
Result for "a"+ at index 0 (aaabb) => ("a")
Result for A at index 0 (aaabb) => [:A "a"]
```

Note that after initiating a bunch of parse subtasks, we start to see some results.  Again, the content in the parentheses is a look ahead at the next several characters in the string, just to get our bearings.  The information after the `=>` is the parse result that was found.  Typically, the parse results are found in reverse order from the order in which the subtasks are initiated, because when initiated, the subtasks are put on a stack.

```
No result for "b" at index 1 (aabb)
```

The tracing mechanism reports when tokens (i.e., strings or regular expressions) are sought but not found.  In general, the tracing mechanism does not report when subtasks involving non-terminals fail (because internally, instaparse does not transmit failure messages between subtasks).

```
Result for S at index 0 (aaabb) => [:S [:AB [:A "a" "a" "a"] [:B "b" "b"]]]
Successful parse.
```

At the end, we see the final parse, followed by some profiling data:

```
Profile:  {:push-message 21, :push-result 21, :push-listener 24, :push-stack 26, :push-full-listener 2, :create-node 26}
```

The details of the profiling data don't matter that much, other than to know that it's a measure of how much work instaparse had to do to come up with the result.  Repeating the trace with an input of `"aaaaaabbbb"` we get the profiling results:

```
Profile:  {:push-message 40, :push-result 40, :push-listener 48, :push-stack 50, :push-full-listener 2, :create-node 50}
```

The key here is that we doubled the length of the input string, and this doubled-the amount of work that instaparse needed to do.  That's good, it means that this parser behaves linearly with respect to its input size.  Even though the code is instrumented with tracing functionality, you still need to explicitly request the trace each time.  If you don't request the trace, it won't display:

```
=> (as-and-bs "aaabb")
[:S [:AB [:A "a" "a" "a"] [:B "b" "b"]]]
```

Now let's look at an example with negative lookahead.  Here is the parser:

```
=> negative-lookahead-example
S = !"ab" ("a" | "b")+
=> (negative-lookahead-example "aabb")
[:S "a" "a" "b" "b"]
```

Let's run it with the trace:

```
=> (negative-lookahead-example "aabb" :trace true)
Initiating full parse: S at index 0 (aabb)
Initiating full parse: !"ab" ("a" | "b")+ at index 0 (aabb)
Initiating parse: !"ab" at index 0 (aabb)
Initiating parse: "ab" at index 0 (aabb)
No result for "ab" at index 0 (aabb)
Exhausted results for "ab" at index 0 (aabb)
Negation satisfied: !"ab" at index 0 (aabb)
Initiating full parse: ("a" | "b")+ at index 0 (aabb)
Initiating parse: "a" | "b" at index 0 (aabb)
Initiating parse: "b" at index 0 (aabb)
No result for "b" at index 0 (aabb)
Initiating parse: "a" at index 0 (aabb)
Result for "a" at index 0 (aabb) => "a"
Result for "a" | "b" at index 0 (aabb) => "a"
Initiating parse: "a" | "b" at index 1 (abb)
Initiating parse: "b" at index 1 (abb)
No result for "b" at index 1 (abb)
Initiating parse: "a" at index 1 (abb)
Result for "a" at index 1 (abb) => "a"
Result for "a" | "b" at index 1 (abb) => "a"
Initiating parse: "a" | "b" at index 2 (bb)
Initiating parse: "b" at index 2 (bb)
Result for "b" at index 2 (bb) => "b"
Result for "a" | "b" at index 2 (bb) => "b"
Initiating parse: "a" | "b" at index 3 (b)
Initiating parse: "b" at index 3 (b)
Result for "b" at index 3 (b) => "b"
Result for "a" | "b" at index 3 (b) => "b"
Result for ("a" | "b")+ at index 0 (aabb) => ("a" "a" "b" "b")
Result for !"ab" ("a" | "b")+ at index 0 (aabb) => ("a" "a" "b" "b")
Result for S at index 0 (aabb) => [:S "a" "a" "b" "b"]
Successful parse.
Profile:  {:push-message 12, :push-result 12, :push-listener 14, :push-stack 17, :push-full-listener 3, :create-node 17}
[:S "a" "a" "b" "b"]
```

The interesting thing with negative lookahead (or ordered choice) is the following lines:

```
Initiating parse: !"ab" at index 0 (aabb)
Initiating parse: "ab" at index 0 (aabb)
No result for "ab" at index 0 (aabb)
Exhausted results for "ab" at index 0 (aabb)
Negation satisfied: !"ab" at index 0 (aabb)
```

To do negative lookahead, the parser sets up a subtask to try to parse the very thing we want to avoid.  If the parser runs out of work to do, then the trace tells us that the negation was in fact satisfied.

When you are done tracing, you probably will want to recompile the code without all the tracing and profiling instrumentation.  You can either restart the REPL or just type:

```
=> (insta/disable-tracing!)
nil
```
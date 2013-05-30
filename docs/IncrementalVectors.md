# Incremental Vectors

Incremental vectors are a drop-in replacement for Clojure's vectors, designed to solve the performance problems that occur with large vectors in applications with a heavy interleaving of modification and hashing.  It exists as part of the instaparse library because I created it to address a performance issue with instaparse, but it is not directly related to parsing and can easily be used independently of the other components of the instaparse library.

## The Problem

Let's start off with a large vector, large enough that the timing differences will be really obvious.

	=> (def v (vec (range 10000000)))

When you hash a vector, it traverses the entire vector to compute the hash.

	=> (time (hash v))
	"Elapsed time: 323.813229 msecs"
	-745145535

Then, it caches the hash result so that computing the hash again is extremely fast.

	=> (time (hash v))
	"Elapsed time: 0.041053 msecs"
	-745145535

Now, let's tack one more item onto the end of the vector.

	=> (def new-v (conj v 666))

Uh oh, when we hash this newly built vector, Clojure's hashing process doesn't take into account that we've already computed the hash on ten million of these elements and have only added on one new item.  Once again, it walks the entire vector.

	=> (time (hash new-v))
	"Elapsed time: 324.556382 msecs"
	-1624674439

The same issue occurs if we pop or assoc or do anything that creates a new, modified vector.  Hashing will cause the entire vector to be traversed.

In certain contexts, this behavior can be a real performance-killer.

## The Solution

Include the `instaparse` library in your dependencies (see README.md for details).

	=> (use 'instaparse.incremental-vector)

Use `ivec` rather than `vec` to create a vector that uses an incremental approach to hashing.

	=> (def v (ivec (range 10000000)))

The hash of `v` was already computed while the vector was being built.

	=> (time (hash v))
	"Elapsed time: 0.033122 msecs"
	-745145535

Let's stick an item on the end of v.

	=> (def new-v (conj v 666))

Again, the hash is automatically updated when the item is added.

	=> (time (hash new-v))
	"Elapsed time: 0.034988 msecs"
	-1624674439

This also works with assoc and pop.

	=> (time (hash (assoc v 1000 555)))
	"Elapsed time: 0.104032 msecs"
	950571422

	=> (time (hash (pop v)))
	"Elapsed time: 0.034989 msecs"
	-717096194

The hash codes of an `ivec` always match that of the corresponding `vec` and ivecs and vecs can be compared for equality.
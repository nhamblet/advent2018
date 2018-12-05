## Day 4: Repose Record

Fun day. Played with some data modeling (and definitely didn't get it really nice,
but it did work), and some folding. I was happy to find that the solution for part 2
was only a few lines different than part 1.

[My solution](day.hs)

### Commentary

I got both parts wrong on my first try, but was able to diagnose my error by trying
again with the sample data and by piecing through the code I had written to find my bug.
In the first part, I had an off-by-one error (counted the awake minute incorrectly),
and in the second part I was using an `(Int, Int)` in the wrong order.

Given the algebraic data types I started off with, and the tuple mixup, I'd be curious
to re-write the models using record syntax, and see what that did to the readability.
I think in this case it might work well - I think I've read it's hard to case-match on,
but I think here it'd work because we don't access most of the fields in the data types.

I continue to find [hoogle](https://www.haskell.org/hoogle/) incredibly useful,
today turning up `sortWith`.

## Day 4: Repose Record

Fun day. Played with some data modeling (and definitely didn't get it really nice,
but it did work), and some folding. I was happy to find that the solution for part 2
was only a few lines different than part 1.

[My solution](day.hs)

I changed up the overall pattern, relative to the earlier days, particularly around
having a `main`. That has seemed to be an extra, un-necessary step, and I've just
started hard-coding paths. It's also fun to write each piece of the solution as you
work through it, and then at the end tie them all together with a large function
composition. What I might try, if I think about it right next time, is actually
writing that composition first, as I write the method signatures, and then go back
and fill in the definitions.

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

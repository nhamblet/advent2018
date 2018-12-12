## Day 11: [Chronal Charge](problem.md)

I almost didn't get the second part of today's puzzle, but eventually got there.
Along the way I looked at [memoization](https://hackage.haskell.org/package/memoize-0.8.1/docs/Data-Function-Memoize.html),
and added [vector](http://hackage.haskell.org/package/vector-0.12.0.1/docs/Data-Vector-Unboxed.html)
to the list of things I should read more about and try some day
(also carrying [array](http://hackage.haskell.org/package/array-0.5.2.0/docs/Data-Array.html)
in my backlog). I tried
[parallelization](https://stackoverflow.com/questions/5606165/parallel-map-in-haskell)
again, but didn't mess with `stack ghci` enough to make it seem to really work
in that context, and didn't mess with actually compiling a script with a main that
I could later run. Maybe I'll circle back and try that another time.

[This thread](https://www.reddit.com/r/haskell/comments/5yiusn/when_you_should_use_lists_in_haskell_mostly_you/)
definitely resonates with me. It's from a year ago, I wonder if thing have improved
and I'm just not doing it right, or something.

Anyway, without too much more ado because it's late, [my solutions](day.hs)


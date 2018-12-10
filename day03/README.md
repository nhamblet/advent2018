## Day 3: [No Matter How You Slice It](problem.md)

I struggled on the first part of this one, but learned some things, and was eventually
able to get both parts. Some of the narrative below isn't quite right, because the
rollup-based attempt 3 actually does work, it's just slow (but not all-night slow).

[My solution](day3.hs)

### Part 1

Given a bunch of boxes, described by upper-left corner coordinates and widths and heights,
determine the number of locations where at least two boxes overlap.

I ended up writing several failed attempts at this, and eventually got to one that works.
Along the way, I remembered a [tweet](https://twitter.com/fommil/status/1062677692376666114) about
adding debugging messages, so threw a few `traceShow`s in. Fun stuff!

#### Attempt 1

First, I needed to find a 2-d array package. I ended up with
[`Data.Matrix`](https://hackage.haskell.org/package/matrix-0.2.2/docs/Data-Matrix.html) from `matrix`,
even though it probably isn't quite tailored for this problem. I end up creating a function at
each coordinate that sums up a 1 or 0 for every box indicating if it hits that coordinate or not.

I also needed a way to parse the lines. I'm [guess](https://two-wrongs.com/parser-combinators-parsing-for-haskell-beginners.html)
I'm supposed to learn about parser combinators, but in my 5-10 minutes trying to suss it out, I
didn't make enough progress, so I fell back on [regexes](http://hackage.haskell.org/package/regex-compat-0.95.1/docs/Text-Regex.html).

Then I had enough to crank along, so kicked off my script. About 9 minutes and 6G of memory later
(about all my laptop could handle), the process got killed.

#### Attempt 2

Ok, so, instead of building up the whole matrix and filling it in, I figured I could walk through
the coordinates, and for each coordinate walk through the boxes until I either found that two
boxes overlapped, or ran out of boxes. I'd keep a tally of all the coordinates with an overlap,
and when I got to the end of coordinates, I'd have my answer.

This code was "smaller", not only shorter but also independent of the `matrix` library (although,
having started with that library, now I'm referring to all my coordinates with 1-indexed counting).
It ran for 20 minutes, but with a pretty small memory footprint. Unfortunately, the final answer
it spit out was apparently too low, and I haven't tracked down my bug yet.

#### Attempt 3

Since I couldn't find my bug, I thought I'd try another variant of a solution. This one goes back
to using the `matrix` library, now "paint"ing coordinates as it works through the boxes. Any time
we go to paint a cell that already had paint in it, we increment a counter. When we're done going
through the boxes, we've got our count ready as the final answer.

Somewhere in here I remembered and dug back up `catMaybes :: [Maybe a] -> [a]`, which removes all
the `Nothing`s.

This seemed like it should be the most efficient approach, but it ended up running all night and
still not finishing.

#### Attempt 4

I finally realized that all of the boxes are small relative to the size of the world they live in,
and that the total size of the list of coordinates they hit isn't too big, so in my final working
solution I produce all the indices they hit, tabulate those (using `count` from `Unique`, like
in [day 2](../day02)), and filter those that are bigger than 1.

Not only was this answer finally correct, it was nice and quick to run!


### Part 2

Now the goal is to find the one box from the inputs that doesn't overlap any other boxes.

This one went much better, which was nice. I took the list of indices from before, and made it
a map from the coordinate to the number of boxes. I can then tell if a box has overlaps by
going through all of its coordinates, and seeing if they're all 1.

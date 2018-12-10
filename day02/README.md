## Day 2: [Inventory Management System](problem.md)

### Part 1

Given a list of strings, count the number of strings that have a letter
that occurs twice, and the number of strings that have a letter that occurs
three times, and multiply those counts together.

[My solution](part1.hs)

#### Lessons learned

* I still haven't used `let` in the right ways, most of the times I've done it.
    Today, I thought I could put a `let` in the body of a `where`, but apparently
    that's a no-no.
* I looked on [hoogle](https://www.haskell.org/hoogle/) for what ended up being
    the `count` function from `Unique`, and was surprised not to find it. I think
    I dug up some other stackoverflow or something to actually find it.


### Part 2

Given a list of strings, find the pair of strings that are off by exactly
one character, and for that pair show the letters that don't disagree.

[My solution](part2.hs)


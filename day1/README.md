## Day 1 - Chronal Calibration

### First Part

The first part is the sum of all the inputs. I copied my input stream to a text file
(e.g., `/tmp/1.1`), and then `echo`-ed a 0 to the front and piped it through a
calculator. I tried `bc`, but got a syntax error I haven't tracked down, so then I
fell back on `stack ghci` :)

### Second Part

It took me a few reads to figure out the examples, but eventually I got it. I thought
the first repeated value was supposed to be while it was running through the list of
frequencies, but it could be many cycles in.

So, intuitively, we cycle through the list of diffs, keeping a running set of the
observed cumulative sums seen so far, and for each new sum, we see if the value is
in the list of previously seen sums.

I wrote up [my solution](part2.hs) in haskell, and provided some sample values from
the example inputs. This was a good excuse to play with using `stack` to run
haskell scripts, and passing them arguments. I
[learned](https://haskell-lang.org/tutorial/stack-script)
to use something like: `stack runghc -- part2.hs input4`.


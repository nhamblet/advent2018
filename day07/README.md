## Day 7: The Sum of Its Parts

In the first part of this problem, we are given a dependency graph, and need to
identify an order-preserving way to walk through the graph. It took me a little
pondering through to figure out what data representations I wanted, and related
functions, but it was fun to work through. I definitely found use in
`Debug.Trace.traceShow` again.

After the first part, the second part involves walking the graph with multiple
parallel workers, which was fun to think about. Originally I tried being a little
too clever, stepping from one timestep to the next time a job would finish
(because each job takes different amounts of time), but it was late and I re-wrote
it to take one time step at a time, and got the answer quickly after that. While I
should use better variable names, the lines in `part2'` look practically imperative,
so somewhat intuitive to read.

[My solutions](day.hs)

### Haskell learnings / ponderings

I'm confident that the code I've got would be more readable with some refactoring,
choosing good data types, and perhaps some day I'll get/make some additional free time
to play with that. I definitely found meaningful function names to be incredibly
useful for thinking through things.

I learned that if you have multiple guard statements, a where clause is associated
with the collection of them, that you can't have independent where clauses for each.
I also learned that if you write a function with pattern matching, all the partial
definitions of the function have to have the same number of arguments to pattern
match. I'm a little surprised about this, but I'm sure there are entertaining examples
where it matters, where it'd be impossible for the compiler to figure out what you
wanted.

I tried using qualified imports for everything, which I sort of appreciate, besides
that I'm not good enough with vim (the editor I'm using) to efficiently bounce
up to the imports and then back down to where I was, to add another import. Also
I ended up with `Data.Maybe` and `Data.Map`, so my 1-letter module shortcuts
failed me. I guess in that case a 2-letter one wouldn't be any better.

While I'm able to write these scripts, with a little googling and hoogling and
ugliness, I'd love to read some more advanced haskell solutions, so might go
searching for repos someday.


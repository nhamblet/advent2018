## Day 9: [Marble Mania](problem.md)

Today was fun. I pondered for a bit in how to represent the game state,
because circular data structures are like that. I decided to try something like the
'zipper' I did for the earlier string problem, maintaining two lists and the 'current marble',
and then allowing myself to move left or right from that state. Being a circle, if
I try to move left but there's nothing there, I can move the things on the right
over to the left if I reverse them.

[My solution](day.hs)

Coming off yesterday's notion that more data types were good, I tried it today,
and it did seem pretty natural. I tried record syntax, I'm still not sure if I like
it better. I'm tempted to write it again without that, to compare. I did learn that
you can [partially case match record syntax](https://stackoverflow.com/questions/38052553/haskell-record-pattern-matching),
which seems pretty cool.

I ended up writing my solutions again, [without record syntax](day-noRecord.hs).
This seems to make the left-hand side of function definitions longer, but then
the right-hand sides shorter. I can't say I tested things very carefully, but I
must note that my challenge problem only took about 22 seconds to run when not
using record syntax, vs 35 seconds to run with record syntax.

I wondered if it might get ugly to try to
[get rid of all the partial functions](day-noRecord2.hs), so I did that also,
and it actually ended up not complicating anything, generally probably making
things easier to think about. It did add about a second to the runtime, but maybe
that's a statistical anomaly :) While I was at it, I also decided to get rid of
all the compiler warnings, so there's a few silly lines associated with the "samples",
but otherwise things were improved with that.

The timing stats above were from `:set +s` in `ghci`. I also realized they include
some notion of "bytes", and was impressed that it ended up as 11 billion (11G?).
Seems like there's probably some efficiencies I could look for :)

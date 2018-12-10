## Day 9: Marble Mania

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


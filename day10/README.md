## Day 10: [The Stars Align](problem.md)

Another fun problem. As I read the problem, I initially thought I might be
setting up a small "print the next one and read it" until I saw a reasonable answer.
Then I noticed that my inputs started in the ten thousands, and I didn't want to
mess with displaying that on my screen. I realized that I could probably guess which
step was the one with the message by identifying how many of the points had a
neighboring point. This worked for the sample input, so I went about running it
for my problem input. Unfortunately, after several hundred runs, the number of
points with neighbors hadn't changed, so I was getting antsy. I had a guess that
I could stop once I found a local maximum, but wasn't sure. Then I noticed,
just on visual inspection, that my input data seemed to have the property that
many of the points started at coordinates that were about -10000 times the velocities.
Like, if the velocity was <2,-1> the point would start at <-2000,1000>. This sorta
shows how the problem input was made up (pick a drawing near 0, and then play it
backwards about 10000 times. So I jumped my diagram forward 9500 steps, and ran it
for 1000 steps, and still my inputs didn't change. Then I got nervous. I checked
some of the ratios a little more carefully, and decided they were actually more like
10500, so I ran the loop from there, and found my answer relatively shortly after that.
The stopping heuristic I had identified would have worked, but it was also nice
to be able to quickly skip 10000 one-step iterations. That said, if I had set up
a loop to stop when the number of neighbors went down, it probably would have taken
my solution less than 20 minutes to run.

My solution doesn't include all the code I ran, since it was a little more manual
today. But all the pieces are there. I didn't try anything to try to identify the
letters from the message automatically.

After the fact, I went back and implemented the automated stopping criteria. Instead
of using the number of neighbors, as described above, I used the area of the minimal
region covered by the points. It would be feasible to mess with this heuristic, because
you could actually start from a nice picture and then have every point's velocity
be in the direction of the centroid of the nice picture. That'd bring the area down,
but would mess up the letters. However, I ran it, and it finished in less than 5
seconds with the right answer, which was kinda awesome. Plus I went back and oriented
the printout so it was easier to read.

[My solution](day.hs)

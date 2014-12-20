Cardinality Estimation Algorithms in Haskell
---
Earlier this year, I watched a talk given by Avi Bryant entitled [Add ALL the things](http://www.infoq.com/presentations/abstract-algebra-analytics)
which introduced me to the HyperLogLog algorithm in particular and cardinality estimation generally. It made an
impression on me along with an excellent series of [blog posts](http://research.neustar.biz/tag/hll/) by a number
of folks at Neustar (formerly Aggregate Knowledge) and inspired me to implement a couple of cardinality algorithms in
Haskell as a project at [Hacker School](http://hackerschool.com).

The posts from Neustar referenced [KMV](http://research.neustar.biz/2012/07/09/sketch-of-the-day-k-minimum-values/)
(k minimum values) a number of times in discussing HLL and mentioned it was a simpler algorithm so I decided to start
by implementing it first. The two represent points along the history of cardinality estimation as an area of research
and make an interesting contrast in terms of approach (order statistics observables -- KMV -- vs. bit pattern
observables -- HLL).

Although a number of implementations of HLL exist (including one in [Haskell](https://hackage.haskell.org/package/hyperloglog)),
I wanted to implement my own to gain a deeper understanding of how these algorithms work. In both cases it has made the
difference between abstractly understanding something and concretely getting it and I recommend it to anyone who would
like to know more about how they tick.

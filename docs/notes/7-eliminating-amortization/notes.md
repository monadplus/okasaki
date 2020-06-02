# 7 Eliminating Amortization

In some situations, a worst-case data structure will often be preferable to an amortized data structure, even if the amortized data structure is simpler and faster overall. Such application areas, including:

- Real-time systems

- Parallel systems: If one processor in a synchronous system executes an expensive operation while the other processors execute cheap operations, then the other processors may sit iddle until the slow processor finishes.

- Interactive systems: users might prefer 100 1-second response times to 99 0.25-second response times and 1 25-second response time.

**Scheduling**: a technique for converting lazy amortized data structures to worst-case data structures by systematically forcing lazy components in such a way that no suspension ever takes very long to execute. Sheduling extends every object with an extra component, called a **schedule**, that regulates the order in which the lazy components of that object are forced.

## 7.1 Scheduling

Amortized and worst-case data structures differ mainly in when the computations charged to a given operation occur.

**intrinsic cost** of a suspension to be the amount of time it takes to force the suspension under the assumption that all other suspensions on which it depends have already been forced  and memoized, and therefore each take only O(1) time to execute.

The **first step** in converting an amortized data structure to a worst-case data structure is to **reduce the intrinsic cost of every suspension to less than the desired bounds. Usually, this involves  rewriting expensive monolithic functions to make them incremental**, either by changing the underlying algorithms slightly or by switching from a representation that supports only monolithic functions, such as suspended lists, to one that supports incremental functions as well, such as streams.

It may happen that a suspensions depends on another suspension, which in turn depends on a third, and so on. If none of the suspensions have been executed previously, then forcing the first suspension results in a cascade of forces.

The **second step** is converting an amortized data structure to a worst-case data structure is to **avoid cascading forces by arranging that, whenever we force a suspension, any other suspensions on which it depends have already been forced and memoized**. then, no suspension takes longer than its intrinsic cost to execute. We acoomplish this by systematically **scheduling** the execution of each suspension so that each is ready by the time we need it.

Every operation, in addition to whatever other manipulations it performs on an object, forces the first few suspensions in the schedule.

Depending on the data structure, maintaining the schedule can be non-trivial.

## 7.2 Real-Time Queues.

We convert the amortized banker's queues of section 6.3.2 to worst-case queues.

Real-time queues: all operations O(1) worst-case [HM81].

Function `rotate`

```haskell
-- | Intrinsict cost is O(1).
rotate :: ([a], [a], [a]) -> [a]
rotate ([] , (y:_), acc)     = y : acc
rotate ((x:xs), (y:ys), acc) = x : (rotate (xs, ys, y:acc))
```

```haskell
data Queue a = Queue Stream a -- f
                     [a]      -- r
                     Stream a -- s (schedule that points to the first unevaluated suspension of f)
```

## 7.3 Binomial Heaps

Insertion in O(1) worst-case time.

`insert` was monolithic (representation was list susp). We want incremental `insert`



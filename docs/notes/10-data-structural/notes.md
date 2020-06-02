# 10 Data-Structural Bootstrapping

Problems whose solutions require solutions to (simpler) stances of the same problem.

Example: bootstrap loader, a very tiny, incomplete operating system whose only purpose is to read in and pass control to a somewhat larger, more capable operating systme.

Example: bootstrapping a compiler. Write a very simple, inefficient interpreter for the language. Then, using the interpreter

## 10.1 Structural decomposition [Buc93]

Bootstrapping complete data structures from incomplete data structures.

**Uniformly recursive**: recursive component in each definition is identical to the type being defined.

**structural decomposition**: recursive data structures that are "non-uniform".

```haskell
data Seq a = Nil | Cons a (Seq (a,a))
```

Non-uniform types often supports more efficient algorithms than their uniform cousins.

### 10.1.3 Bootstrapped Queues

...

## 10.2 Structural abstraction [Buc93]

Extends an implementation of collections, such as lists or heaps, with an efficient join function for combining two collections.

Efficient joint is difficult.

Collection that contain other collections as elements.

Then two collection can be joined by simply inserting one collection into the other.

We boostrap queues to support catenation (i.e. append) efficiently. Second, we bootstrap heaps to support merge efficiently.

### 10.2.1 Lists with Efficient Catenation

catenable lists \equiv catenable output-restricted deques.

```haskell
snoc (xs, x) = xs ++ const (x,empty)
```

### 10.2.2 Heaps with Efficient Merging

Assume that we have an implementation of heaps that supports insert in O(1) worst-case time and merge/findMin/deleteMin in O(log n) worst-case time. The skew binomial heaps of Section 9.3.2 are one such implementation. Using **structural abstraction**, we improve the running time of both findMin/merge to O(1) worst-case time.

### 10.3.1 Tries (digital search trees)

### 10.3.2 Generalized Tries [CM95]

Did not follow.

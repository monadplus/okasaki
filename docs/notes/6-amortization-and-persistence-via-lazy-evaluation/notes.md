Classification of costs:

- **Unshared cost**: time of the operation assuming every suspension on the system has already been forced and memoized.

- **Shared cost**: time that it would take to execute every suspension created but not evaluated.
    - **Realized cost**: shared costs for suspensions that are executed during the overall computation.
    - **Unrealized costs**: shared costs for suspensions that are never executed. (don't count in the total cost)

- **Complete cost**: sum of unshared + shared realized cost (actual cost if evaluation was strict).


**Accumulated debt**: initially zero. Every time a suspension is created, we increase the accumulated debt by the shared cost of the suspensions (and any nested suspensions). Each operation then pays off a portion of the accumulated debt.

The **amortized cost** of an operation is the unshared cost of the operation plus the amount of accumulated debt paid off by the operation. we are not allowed to force a suspension until the debt associated with the suspension is entirely paid off.


You find computation that you can't afford to execute yet.

Suspension lifecycle:
- Created
- Entirely paid off
- Executed

We need to proof that 2nd step happens before 3rd step.  If every suspension is paid off before it is forced, then the total amount of debt that has been paid off is an upper bound on the realized shared costs, and therefore the total amortized cost (i.e. total unshared cost + total amount of debt that has been paid off) is an upper bound on the total actual cost (i.e. the total unshared cost plus the realized shared costs).

Reasoning about multiple logical futures as if it were the only one.

From the point of view of a suspension, any logical future that forces the suspension, must pay for the suspension individually.

## 6.3 The Banker's Method

Replace credits with debits.

Debit is suspended work.

When we initially suspend a given computation, we create a number of debits proportional to its shared cost and associate each debit with a location in the object.

The amortized cost of an operation = unshared cost + number of debits discharged by the operation

To prove an amortized bound, we must show that, whenever we access a location, all debits associated with that location have already been discharged.

### 6.3.2 Example: Queues

We prove that every operation runs in O(1) amortied time using the banker's method.

```haskell
type Queue a = (Int, Stream a, Int, Stream a) -- constant time size()
```

We can't wait for the front list to became emptyo to reverse the rear list (not enough time to pay for the reverse).

We periodically rotate the queye by f = f ++ reverse r

When ? reverse is monolithic, set up the computation far enough in advance to be able to discharge all its debits by the time its result is needed. reverse takes |r| steps, so we allocate |r| debits. The earliest the reverse suspension could be forced is after |f| applications of tail, so we rotate the queue when $|r| \approx |f|$ and discharge one debit per operation. We rotate the queue when |r| > |f| to maintain the invariant that $|f| \geq |r|$. Incidentally, this guarantees that $f$ is empty only if $r$ is also empty.

## 6.4 The Physicist's Method

Potential function $\Psi$ that maps each object to a potential representing an upper bound on the accumulated debt (or at least, an upper bound on this object's portion of the accumulated debt). Roughly speaking, the amortized cost of an operation is then the complete cost of the operation minus the change in potential.

If an operation does not pay any shared costs, then the change in potential is equal to its shared cost, so the amortized cost of the operation is equal to its unshared cost.

In the Physicist's method, the amortized cost is the complete cost minus the change in potential, in other words, the unshared cost plus the difference between the shared cost and the change in potential. The amortized cost in the physicist's method can also be viewed as the unshared cost plus the number of debits discharged.

### Physicist vs Bankers

In the banker's method, we are allowed to force a suspension as soon as the debits for that suspension have been paid off, without waiting for the debits for other suspensions to be discharged, but in the physicist's method, we can force a shared suspension only when we have reduced the entire accumulated debt of an object, as measured by the potential, to zero. Since potential measures only the accumulated debt of an object as a whole and does not distinguish between different locations, we must pessimistically assume that the entire outstanding debt is associated with the particular suspension we wish to force. For this reason, the physicist's method appears to be less powerful than the banker's method. However, when it applies, the physicist's method tends to be much simpler than the banker's method.

### 6.4.1 Example: Binomial Heaps

### 6.4.2 Example: Queues

### 6.4.3 Example: Bottom-Up Mergesort with Sharing

## 6.5 Lazy Pairing Heaps

## 6.6 Chapter Notes

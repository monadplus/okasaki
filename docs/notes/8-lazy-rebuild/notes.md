# 8 Lazy Rebuilding

**lazy rebuilding**, a variant of **global rebuilding**.

## 8.1 Batched Rebuilding

**batched rebuild**: postpone rebalancing until after a sequence of updates, and then to rebalance the entire structure, restoring it to perfect balance.

Batched rebuilding yields good amortized time bounds.

## 8.2 Global Rebuilding [Overmars 83]

**Global rebuilding**: eliminate the amortization from batched rebuilding. Exeute the rebuilding transformation incrementally, performing a few steps per normal operation. Running the rebuilding transformation as a coroutine. The tricky part of global rebuilding is that the coroutine must be started early enough that it can finish by the time the rebuilt structure is needed.

Maintaining two copies of each object. Updates on the copy are buffered and executed.

Global rebuilding can be implemented purely functionally.

### 8.2.1 Example: Hood-Melville Real-time Queues

## 8.3 Lazy Rebuilding

Similar to global rebuilding. Example of lazy rebuilding are 6.4.2 physicist queue and 6.3.2 banker's queue.

## 8.4 Double-Ended Queues (deques)

Elements can be inserted/deleted from either end of the queue.

queues + cons/last/init(all except last)

Remark: signature for queues is a strict subset of the signature for deques.

### 8.4.1 Output-Restricted Deques

A queue that supports insertions at both ends, but deletion from only one end.

### 8.4.2 Banker's Deques

The notion of perfect balance is for the **elements to be evenly divided between the front and rear streams**.

We can't afford to restore perfect balance after every operation, we will settle for guaranteeing that neither stream is more than about $c$ times longer than the other, for some constant $c > 1$. Specifically, we mainting the following balance invariant: `|f| <= c|r| + 1  /\  |r| <= c|f| + 1` (+1 to allow singleton deques). Note that both streams are non-empty whenever the deque contains at least two elements. Whenever the invariant would otherwise be violated ,we restore the deque to perfect balance by transferring elements from the longer stream to the shorter stream.


Adapt Banker's queues or Physicist's queues to obtain deques that support every operation in O(1) amortized time. We choose bankers because they are slightly simpler.

Check `check` function

Remark: Because of the symmetry of this implementation, we can reverse a deque in O(1) time by simply swapping the roles of f and r.

Analysis:
  - d(i) be the number of debits on element i of the stream.
  - D(i) = \sum_{j=o}^i d(j)
  - We maintain the invarant for both the front and rear streams:

### 8.4.3 Real-Time Deques

Real-time deques support every operation in O(1) worst-case time.

We obtain real-time deques from the deques of the previous section by scheduling both the front and rear streams.

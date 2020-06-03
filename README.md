# Purely Functional Data Structures - Chris Okasaki

You will find the following data structures implemented in Haskell:

- [Red-black Trees](./Chapter3/RedBlackTree.hs)
- [Batched Queues](./Chapter5/BatchedQueues.hs)
- [Banker's Queues](./Chapter6/BankersQueue.hs)
- [Physicist's Queues](./Chapter6/PhysicistQueue.hs)
- [Real-Time Queues](./Chapter7/RealTimeQueue.hs)
- [Hood-Melville Real-Time Queues]()

And the following experiments:

- [Self-balance in Red-black Trees](./Experiments/RedBlackTrees/Main.hs)
- [Worst-case time analysis in Real-Time Queues](./Experiments/RealTimeQueues/Main.hs)

## Documents

The following documents can be generated using _Latex_:

- [Red-black Trees](./docs/red-black-trees/report.tex)
- [Final Work](./docs/final-work/report.tex)

## Red-black Tree

A red-black tree [1] is a kind of self-balancing binary search tree. Each node of the binary tree has an extra bit for the color (red or black). The color bits are used to ensure the tree remains approximately balanced during insertions and deletions. The balancing on the tree is not perfect, but is good enough to allow it to guarantee searching in O(log n) time.

A detailed explanation at [red-black-trees.pdf](./red-black-trees.pdf)

### References

[1] Leo. J. Guibas and Robert Sedgewick. A dichromatic framework for balanced trees. In IEEE Symposium of Foundations of Computer Science, pages 8-21, October 1978.

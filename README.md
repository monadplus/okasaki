# Purely Functional Data Structures - Chris Okasaki

You will find the following data structures:

- [red-black tree](#red-black-tree)

### Red-black Tree

A red-black tree [1] is a kind of self-balancing binary search tree. Each node of the binary tree has an extra bit for the color (red or black). The color bits are used to ensure the tree remains approximately balanced during insertions and deletions. The balancing on the tree is not perfect, but is good enough to allow it to guarantee searching in O(log n) time.

### References

[1] Leo. J. Guibas and Robert Sedgewick. A dichromatic framework for balanced trees. In IEEE Symposium of Foundations of Computer Science, pages 8-21, October 1978.

# 9 Numerical Representations

Strong analogy between representations of the number $n$ and representations of container objects of size $n$.
- inserting an element ~ Increasing a number
- deleting an element ~ decreasing a number
- combining two containers ~ adding two numbers

This analogy can be exploited to design new implementations of container abstractions:
- choose a representation of natural numbers with certain desired properties and define the functions on the container objects accordingly.

This is called a **numerical representation**.


## 9.1 Positional Number Systems

A **positional number system** [Knu73b] is a notation for writing a number as a sequence of digits b_0...b_{m-1}.
The digit b_0 is called the *least significant digit*.
The digit b_{m-1} is called the *most significant digit*.
We will always write sequences of digits from least significant to most significant.

A number system is said to be **redundant** if there is more than one way to represent some numbers. For example, redundant binary numbers by taking $w_i = 2^i$ and $D_i = \{ 0,1,2 \}$. The decimal number 13 can be written 1011, or 1201, or 1222.
We disallow trailing zeros.

Computer representations of positional number systems:
- dense: simply a list of digits. It must then include information on either the rank (i.e. the index) or the weight of each non-zero digit.
- sparse:

### 9.2.1 Binary Random-Access Lists

Also called one-sided flexible array.

Supports array-like lookup and update functions, as well as the usual cons/head/tail operations on lists.

A BRAL of size n contains a tree for each one in the binary representation of n. The rank of each tree corresponds to the rank of the corresponding digit;

### 9.2.2 Zeroless Representations

One disappointing aspect of binary random-access lists is that the list functions cons/head/tail run in O(log n) time instea of O(1) time.

Zeroless representation to guarantee head in O(1) `head (One (Leaf x): _) = x`

### 9.2.3 Lazy Representations

### 9.2.4 Segmented Representations

The problem with ordinary binary numbers is that carries and borrows can cascade. For example, incrementing 2^k -1 causes k carries in binary arithmetic. Symmetrically, decrementing 2^k causes k borrows.

Segmented binary numbers solve this problem by allowing multiple carries or borrows to be executed in a single step.

Segmented binary numbers group contiguous sequences of identical digits into blocks so that we can execute a carry or borrow on an entire block in a single step.

### 9.3 Skew Binary Numbers

In lazy binary numbers and segmented binary numbers, we haven seen two methods for improving the asymptotic behavior of the increment and decrement functions from O(log n) to O(1).

In this section, we consider a thrid method, which is usually simpler and faster in practice, but which involves a more randical departure from ordinary binary numbers.

In a **skew binary numbers** [Me83, Oka95b], the weigth w_i of the ith digit is 2^{i+1} - 1, rather than 2^i  as in ordinary binary numbers. D_i = {0,1,2}. For example the decimal number 92 could be written 002101 (least-significant digit first).

This number system is redundant, but, if we add the further constraint that only the lowest non-zero digit may be two, then we regain unique representations. Such a number is said to be in **canonical form**.

Theorem 9.1 [Mye83] Every natural number has a unique skew binary canonical form.

### 9.3.1 Skew Binary Random-Access Lists

### 9.3.2 Skew Binomial Heaps

## 9.4 Trinary and Quaternary Numbers

Def. 9.4 Complete k-ary leaf trees
Def. 9.5 k-nomial trees
Def. 9.6 k-ary pennants

The advantage of choosing bases larger than 2 is that fewer digits are needed to represent each number. A number in base k contains approximately log_k n = log_2 n / log_2 k digits. Fir example, base 4 uses approximately half as many digits as base 2. On the other hand , there are now more possible values for each digit, so processing each digit might take longer.

In numerical representations, processinga digit in base k often takes about k+1 steps, so an operation that processes every digit should take about (k+1)*log_k n steps altogether. The following table display values of ...

| K           | 2 3 4 5 6 7 8 |
|-------------|---------------|
|(k+1)/log_2k | 3.00 2.52 2.50 2.58 2.71  ...

This table suggest that numerical representations based on trinary or quaternary numbers might be as much as 16% faster than numerical representations based on binary numbers. In practice, you rarely observe speed-ups.

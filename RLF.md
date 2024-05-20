# Recursive List Functions
## What are list functions?
First of all, we define a list as ordered and finite collection of elements from $ℕ_0$. Then we define $\mathcal{L}$ as the set of all lists, and $\mathcal{L}^k$ as the set of all lists of length $k$.

A list function is a function $f: \mathcal{L} \to \mathcal{L}$ a list as input and returns a list as output.

## The basic list functions
Here are some basic list functions with their domains:

- _*Zero Left*_ - $0_l : \mathcal{L}^k \to \mathcal{L}^{k+1}$

$$
    0_l [1,2,3] = [0, 1,2,3]
$$

- _*Zero Right*_ - $0_r : \mathcal{L}^k \to \mathcal{L}^{k+1}$

$$
    0_r [1,2,3] = [1,2,3,0]
$$

- _*Delete Left*_ - $\Box_l : \mathcal{L}^k \to \mathcal{L}^{k-1}$ with $k \geq 1$

$$
    \Box_l [1,2,3] = [2,3]
$$

- _*Delete Right*_ - $\Box_r : \mathcal{L}^k \to \mathcal{L}^{k-1}$ with $k \geq 1$

$$
    \Box_r [1,2,3] = [1,2]
$$

- _*Successor Left*_ - $S_l : \mathcal{L}^k \to \mathcal{L}^k$ with $k \geq 1$

$$
    S_l [1,2,3] = [2,2,3]
$$

- _*Successor Right*_ - $S_r : \mathcal{L}^k \to \mathcal{L}^k$ with $k \geq 1$

$$
    S_r [1,2,3] = [1,2,4]
$$

## Composing list functions
The composition of two list functions $F$ and $G$ is a new list function $H$ such that $H = FG$. Given a list $l$, the composition $H l$ is equivalent to $F (G l)$.

## Repetition of list functions
Given a list function $F : \mathcal{L} \to \mathcal{L}$, the repetition of $F$ is defined as $H = \langle F \rangle$, $H : \mathcal{L}^k \to \mathcal{L}^p$ with $k \geq 2$:
    
$$
\langle F \rangle [x, Y, z] = \begin{cases} 
                          [x, Y, z] & x = z \\
                          \langle F \rangle F [x, Y, z] & x \neq z
                  \end{cases}
$$

## The power operator
Given a list function $F : \mathcal{L} \to \mathcal{L}$ and $k \in ℕ_0$, the power operator $(F)^k$ is defined as the composition of $F$ $k$ times. For example, $(F)^3 = F F F$.

So given $F : \mathcal{L} \to \mathcal{L}$ and a list $X$

$$
    (F)^k X = F (F (F \ldots (F X)))
$$

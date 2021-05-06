Class Project: Cluster Editing
====

This directory stores a snapshot of my private repository: https://github.com/mogproject/cluster-editing

In this project, I have implemented the same algorithm for solving [Cluster Editing](https://pacechallenge.org/2021/) with two languages, C++ and Haskell.

## Implemented algorithms

- Branch and bound with the following subroutines
  - Reduction
    - Reduce permanent non-edges
    - Reduce true twins
    - Filter editable vertices
  - Lowerbound
    - Degree-based greedy P3 packing
  - Upperbound (processed component-wise)
    - Degree-based greedy matching
    - Clique formation
  - Branch
    - Naive P3 branching

## Performance results

Haskell performs 4x faster than C++. This is perhaps because my C++ implementation might involve unnecessary data copy.

- Tested with a small instance (n=20, m=30)
  - Haskell (threading off): 18s
  - Haskell (threading on): 51s
  - C++: 77s

## Logs

- C++

```
$ time ./cpp/dist/Exact < ./data/exact/exact003a.gr
1 2
1 6
1 11
2 9
2 12
3 11
3 14
3 20
4 7
5 6
5 8
5 19
7 12
7 20
13 17
15 16
16 17
17 18
17 20
./cpp/dist/Exact < ./data/exact/exact003a.gr  76.51s user 0.09s system 99% cpu 1:16.68 total
```

- Haskell (threading off)

```
$ time stack exec exact < ../data/exact/exact003a.gr
1 2
1 6
1 11
2 9
2 12
3 11
3 14
3 20
4 7
5 6
5 8
5 19
6 7
7 20
13 17
15 16
16 17
17 18
17 20
stack exec exact < ../data/exact/exact003a.gr  17.75s user 0.13s system 99% cpu 17.915 total
```

- Haskell (threading on)

```
$ time stack exec exact < ../data/exact/exact003a.gr
1 2
1 6
1 11
2 9
2 12
3 11
3 14
3 20
4 7
5 6
5 8
5 19
6 7
7 20
13 17
15 16
16 17
17 18
17 20
stack exec exact < ../data/exact/exact003a.gr  51.01s user 6.02s system 230% cpu 24.784 total
```


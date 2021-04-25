---
id: utils
title: Utils module
---
# Utils module

The utils module contains several utilities.


## SetUtils

Example of a utility function

### partition of a set

`paritition(s,n)` generates a partition of a set in `n` sets whose union gives that set again

```scala mdoc
import es.weso.utils.SetUtils._

partition(Set(1,2,3),2)
```

### pSet

`pSet(s)` generates the power set of `s`, pairing each subset with its complement.

```scala mdoc
pSet(Set(1,2,3))
```


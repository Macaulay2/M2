# `janettree.hpp` — `JanetTree<MonomType>`

`JanetTree` is the **involutive-division data structure** that makes
bibasis fast. It indexes a set of monomials by variable-degree and lets
the algorithm answer divisibility queries in time proportional to the
target monomial's support — far better than scanning every basis element.

Part of the [`bibasis/`](README.md) subdirectory.

[← bibasis overview](README.md) · [← engine overview](../README.md)

## Node structure

```cpp
namespace BIBasis {

template <typename MonomType>
class JanetTree {
private:
    struct Node {
        typename MonomType::Integer Degree;
        Triple<MonomType>          *CurrentTriple;
        Node                       *NextDegree;
        // ...
    };
};

}
```

Each `Node` carries:

- A **degree** — the index of the variable being branched on at this depth.
- A **triple** ([`triple.hpp`](README.md)) — the (monomial, polynomial,
  non-multiplicative variables) bundle associated with this node.
- A **`NextDegree`** pointer — sibling in the tree at the same depth.

The tree branches on **which variable to introduce next**. A monomial
`x_{i_1} x_{i_2} ⋯ x_{i_k}` traces a path through the tree by visiting
nodes labelled `i_1, i_2, …, i_k` in order.

## What is "involutive"

In Janet division, a basis polynomial has a set of *multiplicative
variables* and a set of *non-multiplicative variables*. A monomial divides
another only when it does so using only multiplicative variables. The tree
indexes which polynomial is responsible for each multiplicative subspace.

## Templated on `MonomType`

The tree is template-parameterised on the concrete monomial class
([`file-monom.md`](file-monom.md)). The compiler emits a specialised
`JanetTree<MonomLex>`, `JanetTree<MonomDL>`, etc. for each ordering used.

## Insertion and lookup

- **Insert** a `Triple` — walk the tree along the monomial's path, allocate
  new nodes if needed, attach the triple at the appropriate leaf.
- **Lookup** a divisor of a target monomial — walk the path, return the
  first triple whose monomial divides the target.

Both are linear in the path length.

## Related

- [`README.md`](README.md) — bibasis overview.
- [`file-monom.md`](file-monom.md) — monomial type parameter.
- [`triple.hpp`](README.md) — triple type stored at nodes.
- [`file-bibasis.md`](file-bibasis.md) — algorithm consumer.

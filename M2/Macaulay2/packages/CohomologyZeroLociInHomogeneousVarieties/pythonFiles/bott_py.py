"""
Bott's algorithm for computing the sheaf cohomology of homogeneous vector
bundles.

This module is called from Macaulay2 (see Cohomology.m2 -> bottPython), which
imports it via Python-in-M2 and calls only bottPy, the entry point below.
S and m (weights and multiplicities of the bundle) and rho, simpleRoots,
posRoots, rootNorms, M (data of the underlying root system) are all built
on the M2 side and passed in as plain Python lists.
"""

import numpy as np
from fractions import Fraction


def reflect(n, p, simpleRoots):
    """Apply the n-th simple reflection to the weight p (in root coordinates)."""
    p = np.array(p)
    r = np.array(simpleRoots[n - 1])
    return p - p[n - 1] * r


def dominant_conjugate(lam, simpleRoots):
    """
    Bring the weight lam into the dominant Weyl chamber by successive simple
    reflections.

    Returns (dominant_weight, k), where k is the number of reflections used;
    its parity determines the degree of the cohomology group in Bott's
    algorithm.
    """
    v = np.array(lam)
    k = 0
    rank = len(lam)
    while not all(x >= 0 for x in v):
        for j in range(1, rank + 1):
            if v[j - 1] < 0:
                k += 1
                v = reflect(j, v, simpleRoots)
                break  # restart the while-loop check with the updated v
    return v, k


def scalar_product(u, v, rootNorms, M):
    """
    Exact (Fraction-based) scalar product of two weights u, v, computed
    using the Cartan matrix inverse M and the root norms rootNorms.
    """
    u = [Fraction(int(x)) for x in u]
    v = [Fraction(int(x)) for x in v]
    rn = [Fraction(int(x)) for x in rootNorms]

    # transformed_u = M * u (matrix-vector product)
    transformed_u = [
        sum(M[i][j] * u[j] for j in range(len(u)))
        for i in range(len(M))
    ]
    total = sum(transformed_u[i] * v[i] * rn[i] for i in range(len(u)))
    return total / Fraction(2)


def weyl_formula(v, rho, posRoots, rootNorms, M):
    """
    Weyl dimension formula: dimension of the irreducible representation with
    highest weight v, as the product over positive roots r of
    <v + rho, r> / <rho, r>.
    """
    v_plus_rho = [wi + ri for wi, ri in zip(v, rho)]
    num = []
    den = []
    for r in posRoots:
        rr = scalar_product(r, r, rootNorms, M)
        num.append((2 * scalar_product(v_plus_rho, r, rootNorms, M)) // rr)
        den.append((2 * scalar_product(rho, r, rootNorms, M)) // rr)

    N, D = 1, 1
    for x in num:
        N *= int(x)
    for x in den:
        D *= int(x)
    return Fraction(N, D).numerator


def bottPy(S, m, rho, simpleRoots, posRoots, rootNorms, M, d):
    """
    Main entry point, called from Macaulay2.

    Computes the cohomology of a homogeneous vector bundle via Bott's
    algorithm: for each weight l in S (with multiplicity m[i]),
      - if l + rho is singular (lies on a wall of the Weyl chambers), it
        contributes nothing;
      - otherwise l + rho is conjugated to a dominant weight l1 by k simple
        reflections, and the Weyl dimension formula gives the dimension h
        of the corresponding representation, added to H[k].

    Returns H, a list of length d + 1 with H[q] = dimension of H^q.
    """
    H = [0] * (d + 1)
    for i in range(len(S)):
        l = S[i]
        l_plus_rho = [wi + ri for wi, ri in zip(l, rho)]
        if not any(l_plus_rho[j] == 0 for j in range(len(l))):
            l1, q = dominant_conjugate(l_plus_rho, simpleRoots)
            h = weyl_formula(
                [wi - ri for wi, ri in zip(l1, rho)], rho, posRoots, rootNorms, M
            )
            H[q] = H[q] + m[i] * h
    return H

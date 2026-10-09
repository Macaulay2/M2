"""
Decomposition of the tensor product of two irreducible representations.

This module is called from Macaulay2 (see LieFunctions.m2 -> tensorProduct),
which imports it via Python-in-M2 and calls only tensorProduct, the entry
point below. l1, l2 are the two highest weights, simpleRoots and rho come
from the root system, W is the (already computed) weight multiplicity table
of the representation with highest weight l1, and tau is a reduced word
(sequence of simple-reflection indices) for the longest element of the Weyl
group, used to speed up the dominance checks via reflect_sequence.
"""

from math import floor
import numpy as np


def reflect(n, p, simpleRoots):
    """Apply the n-th simple reflection to the weight p (in root coordinates)."""
    p = np.array(p)
    r = np.array(simpleRoots[n - 1])
    return p - p[n - 1] * r


def reflect_sequence(L, p, simpleRoots):
    """Apply the simple reflections listed in L to p, in reverse order."""
    q = np.array(p, dtype=int)
    for i in reversed(L):
        q = reflect(i, q, simpleRoots)
    return q


def dominant_conjugate(lam, simpleRoots):
    """
    Bring the weight lam into the dominant Weyl chamber by successive simple
    reflections.

    Returns (dominant_weight, k), where k is the number of reflections used
    (its parity gives the sign of the weight's contribution below).
    """
    v = np.array(lam)
    k = 0
    rank = len(lam)
    while not all(x >= 0 for x in v):
        for j in range(1, rank + 1):
            if v[j - 1] < 0:
                k += 1
                v = reflect(j, v, simpleRoots)
                break  # restart the outer while-loop with the new v
    return v, k


def tensorProduct(l1, l2, simpleRoots, rho, W, tau):
    """
    Main entry point, called from Macaulay2.

    Computes the weight multiplicities of the tensor product of the
    irreducible representations with highest weights l1 and l2, given the
    weight table W of the l1-representation (each entry w: m0 meaning the
    weight w has multiplicity m0). l2 (bound to `l` below) is the highest
    weight for which the character is not yet known.

    For every weight w of W, all Weyl-group conjugates v of w are visited
    (via an explicit stack instead of recursion, mirroring Klimyk's
    algorithm), and each visited weight is "checked": w + l + rho is
    conjugated back to the dominant chamber, and if it lies strictly inside
    that chamber, its multiplicity m0 (with an alternating sign given by
    the number of reflections used) is added to the result. tau is used as
    a shortcut to check a second, related weight (tau * w) at each step.

    Returns a dictionary {weight tuple -> multiplicity}, with zero entries
    removed.
    """
    l = l2  # the highest weight whose character is being computed
    wStack = {}
    indexStack = {}
    levelStack = {}
    T = {}

    for w in list(W.keys()):
        m0 = W[w]
        sp, i, level = 1, 1, 0
        v = tuple(-x for x in w)
        v, q = dominant_conjugate(v, simpleRoots)
        maxLevel = floor(q / 2)
        maxTau = floor((q - 1) / 2)

        # push (w, i, level) onto the stack
        wStack[sp] = w
        indexStack[sp] = i
        levelStack[sp] = level
        sp += 1

        zero_weight = tuple(0 for _ in range(len(l)))
        if w == zero_weight:
            T[tuple(l)] = T.get(tuple(l), 0) + m0
            continue

        # check(w, m0): does w + l + rho land in the (open) dominant chamber?
        y = tuple(w[j] + l[j] + rho[j] for j in range(len(l)))
        y, q = dominant_conjugate(y, simpleRoots)
        if all(y[j] > 0 for j in range(len(l))):
            key = tuple(y[j] - rho[j] for j in range(len(l)))
            T[key] = T.get(key, 0) + ((-1) ** q) * m0

        # check(tau * w, m0)
        yw = reflect_sequence(tau, w, simpleRoots)
        y = tuple(yw[j] + l[j] + rho[j] for j in range(len(l)))
        y, q = dominant_conjugate(y, simpleRoots)
        if all(y[j] > 0 for j in range(len(l))):
            key = tuple(y[j] - rho[j] for j in range(len(l)))
            T[key] = T.get(key, 0) + ((-1) ** q) * m0

        # depth-first walk (via explicit stack) over the remaining Weyl
        # conjugates of w, up to distance maxLevel from w
        while sp > 1:
            sp -= 1
            w = wStack[sp]
            i = indexStack[sp]
            level = levelStack[sp]
            while i <= len(l) and level < maxLevel:
                if w[i - 1] > 0:
                    v = reflect(i, w, simpleRoots)
                    # only descend if w is a predecessor of v in this branch
                    if all(v[j - 1] >= 0 for j in range(i + 1, len(l) + 1)):
                        if i < len(l):
                            # push (w, i+1, level) to resume this branch later
                            wStack[sp] = w
                            indexStack[sp] = i + 1
                            levelStack[sp] = level
                            sp += 1
                        level += 1

                        # check(v, m0)
                        y = tuple(v[j] + l[j] + rho[j] for j in range(len(l)))
                        y, q = dominant_conjugate(y, simpleRoots)
                        if all(y[j] > 0 for j in range(len(l))):
                            key = tuple(y[j] - rho[j] for j in range(len(l)))
                            T[key] = T.get(key, 0) + ((-1) ** q) * m0

                        if level <= maxTau:
                            # check(tau * v, m0)
                            yv = reflect_sequence(tau, v, simpleRoots)
                            y = tuple(yv[j] + l[j] + rho[j] for j in range(len(l)))
                            y, q = dominant_conjugate(y, simpleRoots)
                            if all(y[j] > 0 for j in range(len(l))):
                                key = tuple(y[j] - rho[j] for j in range(len(l)))
                                T[key] = T.get(key, 0) + ((-1) ** q) * m0

                        if level >= maxLevel:
                            break
                        w, i = v, 1
                    else:
                        i += 1
                else:
                    i += 1

    return {k: v for k, v in T.items() if v != 0}  # drop zero multiplicities

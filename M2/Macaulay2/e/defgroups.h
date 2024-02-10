/**
    \mainpage Macaulay2 C++ engine documentation

    The Macaulay2 engine implements rings, elements in these rings,
    matrices over these rings, mutable matrices, ring maps, and
    computations including

    - Groebner bases
    - Hilbert functions
    - free resolutions

    The engine is written in C++ over a long period of time, some of it
    before the c++ standard library was available or robust.  The engine
    is being updated to c++17, and eventually to c++20.

    @see rings
    @see matrices
    @see comp
 */

/**
 * \defgroup rings Rings
 * 
 */


/**
 * \defgroup ringinfo Ring Info
 * \ingroup rings
 */

/**
 * \defgroup coeffrings Coefficient Rings
 *  \ingroup rings 
 */

/**
  \defgroup polynomialrings Polynomial Rings 
  \ingroup rings

*/

/**
    \defgroup monordering Monomial Orders

    A *monomial ordering* is a total order on the set of monomials of
    a polynomial ring (or of a commutative monoid), such that if $x^\alpha > x^\beta$
    then for any monomial $x^\gamma$, $x^\alpha x^\gamma > x^\beta x^\gamma$
 */

/**
 * \defgroup matrices Matrices
 * 
 */


/**
 * \defgroup comp Computations
 * 
 */

/**
 * \defgroup gb Groebner Bases
 *  \ingroup comp
 * 
 */

/**
 * \defgroup res Free Resolutions
 *  \ingroup comp
 * 
 */

/**
 * \defgroup hilb Hilbert Functions
 *  \ingroup comp
 * 
 */

#ifndef BOX_ENUM_H
#define BOX_ENUM_H

// HEADER
// ======
#include <stdint.h>

/*
Enumerate lattice points ``vec`` obeying ``H @ vec >= rhs`` and ``|vec_i| <= B``
using Kannan's algorithm.

Prefer to sort the columns of ``H`` so that stricter constraints come first.
(This is automatically done in the .pyx file). See `set_bounds` for the logic.

Parameters
----------
out : int32_t*
    Output buffer. Must be pre-allocated to hold at least max_N_out * dim
    elements. Lattice points are written in row-major order.
N_out : long*
    Written with the number of lattice points found.
N_nodes : long*
    Written with the number of nodes visited in the search tree (including
    the root). For N_hyps=0 (no hyperplane constraints) this equals
    ((2B+1)^(n+1)-1)/(2B).
dim : int
    Dimension of the problem.
B : int
    Box half-width: each component satisfies |vec_i| <= B.
H : int*
    Constraint matrix of shape (N_hyps, dim), in row-major order. Each row
    defines one inequality ``H[i] @ vec >= rhs[i]``.
rhs : int*
    Right-hand side of each hyperplane constraint, of length N_hyps.
    May be NULL when N_hyps == 0.
N_hyps : int
    Number of hyperplane constraints (rows of H).
max_N_out : long
    Maximum number of output lattice points. Enumeration stops early if
    reached.
max_N_nodes : long
    Maximum number of search tree nodes to visit. Enumeration stops early
    if reached.

Returns
-------
int
    Status code:
        0  : success
       -1  : dim > 256 (unsupported)
       -2  : exceeded max_N_out outputs
       -3  : exceeded max_N_nodes
*/
int _box_enum_c(
    int32_t * restrict out,
    long * restrict N_out,
    long * restrict N_nodes,
    int dim,
    int B,
    int * restrict H,
    int * restrict rhs,
    int N_hyps,
    long max_N_out,
    long max_N_nodes
);


// IMPLEMENTATION
// ==============
#ifdef BOX_ENUM_IMPLEMENTATION

#define MAX_SUPPORTED_DIM 256

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// #define DEBUG  // uncomment to enable debug logging
#ifdef DEBUG
    #define DEBUG_LOG(...) fprintf(stderr, __VA_ARGS__)
#else
    #define DEBUG_LOG(...) ((void)0)
#endif

static inline int max_int(int a, int b) {
    return a > b ? a : b;
}
static inline int min_int(int a, int b) {
    return a < b ? a : b;
}

// Kannan vec[i] bound setting helper
static inline int set_bounds(
    int sp,
    int i,
    int dim,
    int N_hyps,
    int B,
    int * restrict H,
    int * restrict rhs,
    int64_t * restrict stack_partial_sum,
    int * restrict abssum,
    int32_t * restrict stack_val_min,
    int32_t * restrict stack_val_len)
{
    /*
    Compute the iteration bounds for component i at stack depth sp, writing
    results into stack_val_min[sp] and stack_val_len[sp].

    Parameters
    ----------
    sp : int
        Current stack depth.
    i : int
        Component index being bounded.
    dim, N_hyps, B, H, rhs : (see _box_enum_c; rhs[j] is the bound for constraint j)
    stack_partial_sum : int32_t*
        Partial dot products ``H @ vec`` for components already fixed.
    abssum : int*
        Precomputed prefix sums of |H[:,k]| for each constraint.
    stack_val_min : int32_t*
        Written with the minimum value to try for vec[i].
    stack_val_len : int32_t*
        Written with the number of candidates to try for vec[i].

    Returns
    -------
    int
        Number of candidates (stack_val_len[sp]).
    */
    int lo = -B;
    int hi =  B;

    // cut by each hyperplane
    for (int j=0; j<N_hyps; ++j) {
        /*
        Imposes constraint dot(H[j,:], vec) >= rhs[j].

        Split:
            rhs[j] <= dot(H[j,:i],vec[:i])
                    + H[j,i]*vec[i]
                    + dot(H[j,i+1:],vec[i+1:])
        the third term is stack_partial_sum[j], so
            rhs[j] <= dot(H[j,:i],vec[:i])
                    + H[j,i]*vec[i]
                    + stack_partial_sum[j].
        The maximum value possible of dot(H[j,:i],vec[:i]) is
            dot(H[j,:i],vec[:i]) <= B*sum(abs(H[j,:i])).
        Thus
            rhs[j] - stack_partial_sum[j] - B*sum(abs(H[j,:i])) <=  H[j,i]*vec[i].

        The term B * sum(abs(H[j,:i])) is the 'slack'. It is where any looseness
        in our bounds can arise from. Ideally, then, you'd want
        sum(abs(H[j,:i])) to be minimized to have as tight bounds as possible.

        We obviously can't sort each row/constraint individually since that'd
        muddle the components of vec. What we can do is minimize the net slack,
        sum(abs(H[:,:i])). This is done by ordering *columns* in increasing
        L1-norm, which is the rationale behind the column sorting.
        */
        int h = H[j*dim + i];
        if (h == 0){
            continue;
        }

        // Use int64 to avoid overflow: partial_sum ~ H*B, abssum*B can both exceed 2^31
        int64_t numer = (int64_t)rhs[j]
                      - stack_partial_sum[sp*N_hyps + j]
                      - (int64_t)B * (int64_t)abssum[j*(dim+1) + i];

        if (h>0){
            double v = ceil((double)numer/h);
            lo = max_int(lo, v > (double)hi ? hi + 1 : (int)v);
        } else {
            double v = floor((double)numer/h);
            hi = min_int(hi, v < (double)lo ? lo - 1 : (int)v);
        }
    }

    // store the data to recreate the interval
    int num = 0;
    if (hi >= lo) {
        num = hi - lo + 1;
    }
    stack_val_min[sp] = lo;
    stack_val_len[sp] = num;

    // debug print statement
    DEBUG_LOG("Set bounds for %d to %d->%d+%d\n", sp, lo, lo, num);

    return num;
}

// custom Kannan code for lattice vector generation
int _box_enum_c(
    int32_t * restrict out,
    long * restrict N_out,
    long * restrict N_nodes,
    int dim,
    int B,
    int * restrict H,
    int * restrict rhs,
    int N_hyps,
    long max_N_out,
    long max_N_nodes)
{
    /* (see header doc) */
    // check dimensions are reasonable
    // -------------------------------
    if (dim > MAX_SUPPORTED_DIM) {
        *N_out = 0;
        return -1;
    }

    // define variables
    // ----------------
    int status = 0;

    // define arrays
    int32_t vec[dim];

    int32_t stack_i[dim+1];
    int32_t stack_pos[dim+1];

    int32_t stack_val_len[dim+1];
    int32_t stack_val_min[dim+1];

    int64_t stack_partial_sum[N_hyps*(dim+1)];
    memset(stack_partial_sum, 0, sizeof(stack_partial_sum));

    // output/stack pointer
    long op = 0;
    int sp = 0;

    // misc helpers
    int abssum[N_hyps*(dim+1)];
    for (int j=0; j<N_hyps; ++j) {
        abssum[j*(dim+1) + 0] = 0;

        for (int k=0; k<dim; ++k) {
            abssum[j*(dim+1) + k+1] = abssum[j*(dim+1) + k] + abs(H[j*dim + k]);
        }
    }

    // initialize stack
    // ----------------
    stack_i[sp]   = dim-1;
    stack_pos[sp] = 0;

    int k = set_bounds(
            sp,
            dim-1,
            dim,
            N_hyps,
            B,
            H,
            rhs,
            stack_partial_sum,
            abssum,
            stack_val_min,
            stack_val_len);
    if (k == 0) {
        goto end;
    }

    // iterate over the stack
    // ----------------------
    int i;
    int pos;

    *N_nodes = 1;  // count the root
    while (sp >= 0) {

        // read from the stack
        i    = stack_i[sp];
        pos  = stack_pos[sp];

        // debug print statement
        DEBUG_LOG("Setting component-%d for op=%ld, sp=%d, pos=%d\n",
                  i, op, sp, pos);

        // save if node is complete
        // if i==-1, then we have fully written vec
        if (i == -1) {
            if (op >= max_N_out) {
                status = -2;
                goto end;
            }

            memcpy(&out[op * dim], vec, dim * sizeof(int32_t));

            op++;

            // kill node
            sp--;
            continue;
        }

        // check if we exhausted values for this component
        if (pos == stack_val_len[sp]) {
            sp--;
            continue;
        }

        // set vec[i]
        int veci = stack_val_min[sp] + pos;
        vec[i] = veci;

        DEBUG_LOG("Set     component-%d for op=%ld, sp=%d, pos=%d to %d\n",
                  i, op, sp, pos, veci);

        // advance pos for next iteration
        stack_pos[sp] += 1;

        // passes cuts -> push next depth :)
        sp += 1;
        (*N_nodes)++;
        if (*N_nodes > max_N_nodes) {
            DEBUG_LOG("QUITTING DUE TO TOO MANY NODES/ITERATIONS\n");
            status = -3;
            goto end;
        }
        stack_i[sp]       = i-1;
        stack_pos[sp]     = 0;

        // update the partial sums
        for (int j = 0; j<N_hyps; ++j) {
            int64_t prev = stack_partial_sum[(sp-1)*N_hyps + j];
            stack_partial_sum[sp*N_hyps+j] = prev + (int64_t)H[j*dim + i] * (int64_t)veci;
        }

        if (i > 0) {
            set_bounds(
                sp,
                i-1,
                dim,
                N_hyps,
                B,
                H,
                rhs,
                stack_partial_sum,
                abssum,
                stack_val_min,
                stack_val_len);
        }
    }

    DEBUG_LOG("DONE\n");

    end:
        *N_out = op;
        return status;
}

#undef MAX_SUPPORTED_DIM

#endif // BOX_ENUM_IMPLEMENTATION

#endif // BOX_ENUM_H

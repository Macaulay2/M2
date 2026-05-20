// Copyright 2012  Michael E. Stillman
#ifndef __interrupted__hpp__
#define __interrupted__hpp__

/**
 * @file interrupted.hpp
 * @brief `system_interrupted()` --- thread-safe polling predicate for Ctrl+C handling.
 *
 * Exposes the single global function `system_interrupted()`,
 * which reads the thread-local atomic field
 * `interrupts_interruptedFlag` defined in the system
 * supervisor. The body (in `interrupted.cpp`) is a one-line
 * wrapper around the `interrupted()` macro that pulls the
 * thread-local out of the supervisor's `THREADLOCAL` /
 * `atomic_field` plumbing; the header exists so callers see
 * only the abstract predicate without having to pull in
 * `system/supervisor.hpp` and its atomic-primitive support.
 * Engine inner loops poll the flag and bail out by returning
 * `COMP_INTERRUPTED` (defined in `interface/computation.h`)
 * when it is set, the chain that delivers a user Ctrl+C to a
 * running computation.
 *
 * Polled by long-running engine loops --- the GB and resolution
 * drivers, NAG path tracking, F4 matrix passes, dense
 * linear-algebra routines --- as the cooperative cancellation
 * handshake matching the stop-condition machinery declared in
 * `comp.hpp`.
 *
 * @see comp.hpp
 * @see interface/computation.h
 */

extern bool system_interrupted();

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:

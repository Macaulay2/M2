// Copyright 2004 Michael E. Stillman

#ifndef _comp_hpp_
#define _comp_hpp_

#include "interface/computation.h"
#include "hash.hpp"

class GroebnerBasis;

class GBBComputation;
class ResolutionComputation;

class EngineComputation;
class EngineGBComputation;

class buffer;
class GBComputation;
class GBBComputation;

/**
    @ingroup gb

    @brief Non-functional code.
*/
class GBBComputation : public MutableEngineObject
// This is the base type for all Groebner basis and syzygy computations
{
 protected:
  GBBComputation() {}
 public:
  virtual ~GBBComputation();

  static GBBComputation *choose_gb(
      const Matrix *m,
      M2_bool collect_syz,
      int n_rows_to_keep,
      M2_arrayint gb_weights,  // null defaults to (1,...,1)
      M2_bool use_max_degree,
      int max_degree,
      int algorithm,
      int strategy,
      int max_reduction_count = 10);
  // Values for algorithm and strategy are documented in engine.h
  // Returns NULL if an error occurs

  virtual GBBComputation *set_hilbert_function(const RingElement *h);
  // The default version returns an error saying that Hilbert functions cannot
  // be used.
  // NULL is returned if there is an error...?

  virtual ComputationStatusCode compute(
      const StopConditions &stop,
      long &result_complete_thru_this_degree) = 0;

  virtual int complete_thru_degree() const = 0;
  // The computation is complete up through this degree.

  virtual GroebnerBasis *get_GroebnerBasis() = 0;

  virtual GroebnerBasis *steal_GroebnerBasis() = 0;  // This assumes that the
                                                     // computation is to be
                                                     // destroyed...

  virtual const Matrix /* or null */ *get_gb_snapshot() = 0;

  virtual void text_out(buffer &o) const;
  // This displays statistical information, and depends on the
  // M2_gbTrace value.

  virtual void show() const;
};

class GroebnerBasis : public MutableEngineObject  // mutable, or immutable???
{
 protected:
  GroebnerBasis() {}
 public:
  virtual ~GroebnerBasis() {}  // frees all space associated with this GB
  ////////////////////////////////
  // Results of the computation //
  ////////////////////////////////
  virtual const Matrix /* or null */ *get_gb() = 0;

  virtual const Matrix /* or null */ *get_mingens() = 0;

  virtual const Matrix /* or null */ *get_change() = 0;

  virtual const Matrix /* or null */ *get_syzygies() = 0;

  virtual const Matrix /* or null */ *get_initial(int nparts) = 0;

  virtual const Matrix /* or null */ *get_parallel_lead_terms(M2_arrayint w);

  ////////////////////////////////
  // Normal forms and lifting ////
  ////////////////////////////////

  virtual const Matrix /* or null */ *matrix_remainder(const Matrix *m) = 0;

  virtual M2_bool matrix_lift(const Matrix *m,
                              const Matrix /* or null */ **result_remainder,
                              const Matrix /* or null */ **result_quotient) = 0;

  virtual int contains(const Matrix *m) = 0;

  //////////////////////////////////////
  // Statistics and spair information //
  //////////////////////////////////////

  virtual void text_out(buffer &o) const;
  // This displays statistical information, and depends on the
  // M2_gbTrace value.
};

class EngineComputation : public MutableEngineObject
{
 private:
  enum ComputationStatusCode computation_status;

 protected:
  StopConditions stop_;

  EngineComputation();

  enum ComputationStatusCode set_status(enum ComputationStatusCode);

  virtual ~EngineComputation() {}
  // These are finalized objects, so this function is usually bypassed
 public:
  void set_stop_conditions(M2_bool always_stop,
                           M2_arrayint degree_limit,
                           int basis_element_limit,
                           int syzygy_limit,
                           int pair_limit,
                           int codim_limit,
                           int subring_limit,
                           M2_bool just_min_gens,
                           M2_arrayint length_limit);

  virtual void destroy() = 0;  // This function will be called when
  // the computation is finalized by the garbage collector

  enum ComputationStatusCode status() const { return computation_status; }
  virtual long complete_thru_degree() const = 0;
  // This is computation specific information.  However, for homogeneous
  // GB's, the GB coincides with the actual GB in degrees <= the returned value.
  // For resolutions of homogeneous modules, the resolution
  // coincides with the actual one in (slanted) degrees <= the returned value.

  virtual void start_computation() = 0;
  // Do the computation as specified by the stop conditions.
  // This routine should set the status of the computation.

  virtual EngineGBComputation *cast_to_EngineGBComputation() { return 0; }
  virtual void text_out(buffer &o) const;

  virtual void show() const;  // debug display of some computations
};

class EngineGBComputation : public EngineComputation
{
  GBBComputation *C;
  GroebnerBasis *G;
  long complete_thru_this_degree;  // only valid after computation has been
                                   // initialized?
 protected:
  EngineGBComputation(GBBComputation *C0)
      : C(C0), G(0), complete_thru_this_degree(0)
  {
  }

  EngineGBComputation(GroebnerBasis *G0)
      : C(0), G(G0), complete_thru_this_degree(0)
  {
  }

  virtual ~EngineGBComputation();

 public:
  EngineGBComputation *cast_to_EngineGBComputation() { return this; }
  static EngineGBComputation *create(GBBComputation *C0);

  static EngineGBComputation *create(GroebnerBasis *C0);

  static EngineGBComputation *create(GBComputation *G0);

  // create routine: this finalizes the object
  // on destruction, delete C, delete G are both done.

  // Computation routines
  //   C->XXX()
  //   C->compute(stop_) returns status.
  //     if status is COMP_DONE, then G is set (or reset) from C,
  //     and C is removed.
  // Parts of a GB:
  //   if C is not removed yet, then use C->get_GroebnerBasis()->...
  //   otherwise use G->...

  virtual void destroy();

  EngineComputation *set_hilbert_function(const RingElement *h)
  {
    // The default version returns an error saying that Hilbert functions cannot
    // be used.
    if (C == 0 || C->set_hilbert_function(h)) return this;
    return 0;
  }

  virtual void start_computation();

  virtual long complete_thru_degree() const;
  // The computation is complete up through this degree.

  // Recall that the status of the computation is maintained by the Computation
  // class,

  GroebnerBasis *get_GroebnerBasis()
  {
    if (!G && C) G = C->get_GroebnerBasis();
    return G;
  }
};

/// Older -- but current --- code ///
class Computation : public MutableEngineObject
{
 private:
  enum ComputationStatusCode computation_status;

 protected:
  StopConditions stop_;

  Computation();

  enum ComputationStatusCode set_status(enum ComputationStatusCode);

  virtual bool stop_conditions_ok() = 0;
  // If the stop conditions in stop_ are inappropriate,
  // return false, and use ERROR(...) to provide an error message.

  virtual ~Computation();

 public:
  Computation /* or null */ *set_stop_conditions(M2_bool always_stop,
                                                 M2_arrayint degree_limit,
                                                 int basis_element_limit,
                                                 int syzygy_limit,
                                                 int pair_limit,
                                                 int codim_limit,
                                                 int subring_limit,
                                                 M2_bool just_min_gens,
                                                 M2_arrayint length_limit);
  // returns NULL if there is a general problem with one of the stop
  // conditions.

  enum ComputationStatusCode status() const { return computation_status; }
  virtual int complete_thru_degree() const = 0;
  // This is computation specific information.  However, for homogeneous
  // GB's, the GB coincides with the actual GB in degrees <= the returned value.
  // For resolutions of homogeneous modules, the resolution
  // coincides with the actual one in (slanted) degrees <= the returned value.

  virtual void start_computation() = 0;
  // Do the computation as specified by the stop conditions.
  // This routine should set the status of the computation.

  virtual GBComputation *cast_to_GBComputation() { return 0; }
  virtual ResolutionComputation *cast_to_ResolutionComputation() { return 0; }
  virtual void text_out(buffer &o) const;

  virtual void show() const;  // debug display of some computations
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:

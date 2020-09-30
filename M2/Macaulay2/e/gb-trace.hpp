/* Copyright 2009, Michael E. Stillman */

#ifndef _gb_trace_hpp_
#define _gb_trace_hpp_

class GBList
{
 public:
  GBList(long len);
  ~GBList();

  long append(gbvector *v);

  gbvector *get(long i);
  void put(long i, gbvector *v);
  long allocated();
  long max();
  long offset();
};

/**
    @ingroup gb

    @brief Unfinished code.  Meant to handle tracing of a GB from ZZ/p to QQ.
*/

class GBTrace : public newdelete
{
  // Commands:
  //  spair(a,b)
  //  reduceBy(c)
  //  store (next number in list).
  // One way: a b c1 c2 c3 ... cn 0
  //  gb elements are stored starting at 1.
  //  ring elements are negative integers
  //  0 is not used?
  struct node
  {
    long a;
    long b;
    long *reducers;
  };
  queue<long> trace;

 public:
  GBTrace *create();  // Create an empty trace
  ~GBTrace();

  // Creation of a trace
  void spair(long a, long b);
  void reducer(long c);
  void endpair();

  // Display of a trace
  M2_arrayint get() const;
  void remove() const;  // Destroy the space associated with this trace
};

class GBTracer
{
  VECTOR(gbvector *) basis;

  FreeModule *F;
  GBRing *GR;
  GBTrace *T;

  bool use_heap;
  long loc;  // The next step to do in the computation

  void populate_with_quotient_elements(FreeModule *F);

 public:
  GBTracer(FreeModule *F, GBTrace *T, gbvector **vecs, bool use_heap = true);
  ~GBTracer();

  bool execute(long nsteps = -1);  // same, but do <= nsteps
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:

// Copyright 2010 Michael E. Stillman.

class PolynomialRing;
class MonomialIdeal;
class MutableMatrix;
class GBComputation;
class ResolutionComputation;
class SchreyerOrder;

//* keepAlive: call at the end of a function when using a
//* finialized object.  More specifically: if you are using an
//* iterator into a data structure, or any pointers into such, which
//* are pointed to by the finalized object, call this at the end of
//* the function in question.
//*
//* The reason for this is that the optimizer can potentially elide
//* the finalized object and the GC finalizer might get called
//* otherwise on this object.  The finalizer function might then free
//* the data you are using from underneath you.
//* 
//* This happened for instance in the MonomialIdeal class, in
//* e.g. MonomialIdeal::sat.  If a pointer to the MonomialIdeal is not
//* being kept in the frontend (as a RawMonomialIdeal) then it was
//* happening that the optimizer was optimizing out the local
//* existence of the pointer to this object.  This in turn caused the
//* destructor of the object to be called, which then left the
//* iterator traversing the data structure in a bad state (in fact,
//* all nodes of the data structure were deleted by the destructor).
//* 
//* Adding in e.g. `nopAndKeepAlive(&J)` at the end of
//* MonomialIdeal::sat allows this to not occur.

void keepAlive(const void*);

// These functions should be called if G will not be freed by its owner
void intern_polyring(const PolynomialRing *G);
void intern_monideal(MonomialIdeal *G);
MutableMatrix *internMutableMatrix(MutableMatrix *G);
void intern_GB(GBComputation *G);
void intern_res(ResolutionComputation *G);
void intern_SchreyerOrder(SchreyerOrder *G);

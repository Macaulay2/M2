// Copyright 2010 Michael E. Stillman.

class PolynomialRing;
class MonomialIdeal;
class MutableMatrix;
class GBComputation;
class ResolutionComputation;
class SchreyerOrder;

// These functions should be called if G will not be freed by its owner
void intern_polyring(const PolynomialRing *G);
void intern_monideal(MonomialIdeal *G);
MutableMatrix *internMutableMatrix(MutableMatrix *G);
void intern_GB(GBComputation *G);
void intern_res(ResolutionComputation *G);
void intern_SchreyerOrder(SchreyerOrder *G);

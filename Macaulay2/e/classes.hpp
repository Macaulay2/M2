// Copyright 1998 by Michael E. Stillman

#ifndef _classes_hh_
#define _classes_hh_

// Class structure for the engine.
// and class and type names.
// Class ID's are given to each class that may have
// instances.  These are to be unique, and
// are used to determine equality between objects.


enum {
  TY_IntegerArray,
  TY_String,
  TY_MonomialOrder,
  TY_Monoid,
  TY_MonomialIdeal,
  TY_TermIdeal,
  TY_Ring,
  TY_RingElement,
  TY_FreeModule,
  TY_Matrix,
  TY_RingMap,

  TY_Vector,
  TY_Monomial,

  TY_Computation
};

enum class_identifier {
  CLASS_int,
  CLASS_string,			// object_string
  CLASS_intarray,		// object_intarray

  CLASS_RingElement,
  CLASS_Vector,
  CLASS_Matrix,
  CLASS_MonomialIdeal,
  CLASS_monomial,

  CLASS_TermIdeal,

  CLASS_MonomialOrder,		// This will not be here soon
  CLASS_NewMonomialOrder,	// Same here...
  CLASS_Monoid,

  CLASS_Ring,
  CLASS_PolynomialRing,
  CLASS_WeylAlgebra,
  CLASS_SkewPolynomialRing,
  CLASS_Z,
  CLASS_Z_mod,
  CLASS_GF,
  CLASS_FractionField,
  CLASS_SchurRing,

  CLASS_FreeModule,
  CLASS_WeylFreeModule,

  CLASS_RingMap,

  CLASS_DetComputation,
  CLASS_PfaffianComputation,
  CLASS_GaussElimComputation,
  CLASS_computation,
  CLASS_GB_comp,
  CLASS_binomialGB_comp,
  CLASS_GBinhom_comp,
  CLASS_HermiteComputation,
  CLASS_hilb_comp,
  CLASS_gb_comp,
  CLASS_res_comp,
  CLASS_res2_comp,
  CLASS_gbres_comp,
  CLASS_SagbiComputation
};
//const type_identifier TY_Integer = TY_RingElement;

enum type_identifier {
    TY_NONE, TY_STRING, TY_INTARRAY,
    TY_RING, TY_FREEMODULE, TY_RING_ELEM, TY_VECTOR, TY_MATRIX,
    TY_RING_MAP, TY_MONIDEAL, TY_TERMIDEAL,
    TY_MON_ORDER, TY_NEW_MON_ORDER,
    TY_MONOID, TY_MONOMIAL,
    TY_COMP, 
    TY_HILB_COMP,
    TY_GB_COMP,
    TY_RES_COMP, 
    TY_RES2_COMP,
    TY_GBRES_COMP
};
const type_identifier TY_INT = TY_RING_ELEM;

template <class T> class array;
template <class T> class hashtable;
template <class T> class queue;
template <class T> class stack;

class caster;
class intarray;
class handles;
class primitive;

class object_element;
  class object_int;
  class object_string;
  class object_intarray;

  class type;
    class ring;
      class Z;
      class Z_mod;

    class Monoid;
      class exp_vector;

  class mon_order;
  class new_mon_order;
  class object_mon_order;
  class object_new_mon_order;
  class ring_elem_rec;
  class monomial_rec;
  class freemodule_rec;
  class matrix_rec;
  class monideal_rec;
  class res_pair;
  class res_degree;
  class res_level;
  class res_comp;


class object;

class Monomial;

class varpower;
class int_bag;
typedef int_bag Bag;
class mi_node;

class AssociatedPrimes;


union ring_elem;
//    vec;

class Ring;
  class Z_mod;
  class GF;
  class Z;
  class PolynomialRing;
  class FractionField;
  class SchurRing;
class MonomialIdeal;
class TermIdeal;
class FreeModule;
class RingElement;
class Vector;
class Matrix;
class RingMap;

class gb_comp;
  class GB_comp;
  class NGB_comp;
  class GBinhom_comp;
  class HermiteComputation;
class hilb_comp;
  class computation;
    class DetComputation;
    class PFaffianComputation;
class gb_node;
class gb2_comp;
class gbres_comp;
class res2_comp;
class gb_emitter;

class object_writer;
class object_reader;
#endif


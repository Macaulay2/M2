// (c) 1994 Michael E. Stillman

#ifndef _classes_hh_
#define _classes_hh_

// Class structure for the engine.

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
  class object_mon_order;
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

class monomial;

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

#endif


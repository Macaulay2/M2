// Copyright 1998 by Michael Stillman
#ifndef __monorder_hpp_
#define __monorder_hpp_

#include "Edefs.hpp"

const int MO_LEX = 0;
const int MO_WTLEX = 1;
const int MO_REVLEX = 2;
const int MO_WTREVLEX = 3;
const int MO_WTFCN = 4;
const int MO_GROUP_ADDITIVE = 5;  // Lex order.  Display as {1,2,-3}
const int MP_GROUP_MULT = 6; // Lex order.  Display as x*y^2*z^(-3).
const int MO_NC_LEX = 7;

/////////////////////////////////////////////////////////////
// Binary form for a monomial order:
// MO_MAGIC  word 0
// len       length of the entire array, including magic number.
// nvars     number of variables total
// nblocks   number of blocks
// for each block:
// if of type MO_LEX:
//    MO_LEX
//    nvars in block
// if of type MO_REVLEX:
//    MO_REVLEX
//    nvars in block
// if of type MO_WTREVLEX:
//    MO_WTREVLEX
//    nvars in block
//    weight vector: array 0..nvars in block - 1
// if of type MO_WTFCN
//    MO_WTFCN
//    weight vector: array 0..nvars-1 of integers
// if of type MO_COMPONENT: this is where the component is.
//    MO_COMPONENT
// finally: end with MO_MAGIC_END
/////////////////////////////////////////////////////////////
class EMonomialOrder : public type
{
private:
  struct mon_order_node {
    int typ;			// MO_LEX, MO_REVLEX, MO_WTREVLEX, MO_WTFCN, MO_COMPONENT, MO_GROUP.
    int n;			// number of variables in this block, for MO_LEX, MO_REVLEX
    int nslots;
    int first_exp;
    int first_slot;
    int nweights;		// Length of weights array.
    int *weights;		// weight vector, in the case MO_WTFCN, MO_WTREVLEX, MO_WTLEX
    bool isgroup;		// If true, variables in this group can have negative exponents.
  };

  int nvars;
  int nslots;
  int componentloc;		// If 5, then the component goes between slot 4 and slot 5.

  int nblocks;
  mon_order_node **order;	// An array 0..nblocks-1

  int n_nc_blocks;		// number of non-commutative blocks

  EMonomialOrder();		// Private: The only routine that creates a monomial order is
				// 'make'.

  void append_block(mon_order_node *b);
  void append_block(int nzeros, mon_order_node *b);
  mon_order_node* find_block(int v) const;

public:
  ~EMonomialOrder();

  // Producing monomial orders.  Each of these modifies 'this', and returns 'this'.
  // EXCEPT: make(), clone().
  static EMonomialOrder *make();
  EMonomialOrder *clone() const;

  EMonomialOrder *revlex(int nvars, bool isgroup=false);
  EMonomialOrder *revlexWeights(int nvars, const int *wts, bool isgroup=false);
  EMonomialOrder *component();
  EMonomialOrder *weightFunction(int nvars, const int *wts);
  EMonomialOrder *lex(int nvars, bool isgroup=false);
  EMonomialOrder *product(const EMonomialOrder *mo2);

  EMonomialOrder *NClex(int nvars);

  // Query routines (about the order)
  bool is_revlex() const;  // NOT DONE
  bool is_elimination(int i) const; // NOT DONE
  bool is_product(int i) const; // NOT DONE
  bool is_commutative() const { return n_nc_blocks == 0; }
  bool is_group() const;  // Are the variables all allowed to have negative exponents? NOT DONE

  bool isNonnegativeVariable(int v) const;
  
  // Get features of the monomial order
  int n_vars() const {return nvars;}     // Size of 'expvector's
  int n_slots() const {return nslots;}   // Size of 'partial sums'
  int n_blocks() const {return nblocks;}
  int component_loc() const {return componentloc;}
  int n_slots(int n) const;  // Number of slots used in the first n blocks in the order.
  
  // Encode and decode
  void encode(const int *exp, int *result_psums) const;
  void decode(const int *psums, int *result_exp) const;

  // Non-commutative stuff
  void set_noncommutative_parameters(int &nblocks,
				     int * &nclengths,
				     bool * & isncslots,
				     bool * & is_comm) const;

  // I/O:  All of these routines are in io.cpp
  void text_out(buffer &o) const;
  void bin_out(buffer &o) const;

  int * put_binary() const;  // NOT DONE

  static EMonomialOrder *get_binary(const int *monorder);
  static EMonomialOrder *binary_in(istream &i);

  EMonomialOrder * cast_to_EMonomialOrder() { return this; }
  const EMonomialOrder * cast_to_EMonomialOrder() const { return this; }
  
  class_identifier class_id() const { return CLASS_EMonomialOrder; }
  type_identifier  type_id () const { return TY_EMonomialOrder; }
  const char * type_name   () const { return "EMonomialOrder"; }

};
#endif

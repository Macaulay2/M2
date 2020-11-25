// (c) 1994  Michael E. Stillman
#ifndef _monideal_hh_
#define _monideal_hh_

#include "queue.hpp"
#include "index.hpp"
#include "varpower.hpp"
#include "int-bag.hpp"
#include "ring.hpp"
#include "polyring.hpp"

class MinimalPrimes;

class Nmi_node  // monomial ideal internal node ///
{
  friend class MonomialIdeal;
  friend class AssociatedPrimes;
  friend class MinimalPrimes;

 protected:
  int var;
  int exp;
  Nmi_node *left;
  Nmi_node *right;
  Nmi_node *header;
  enum { node, leaf } tag;
  union
  {
    Nmi_node *down;  // 'up' node, if this is a head of a list
    Bag *bag;
  } val;

  //  Nmi_node(int v, int e, Nmi_node *d)
  //    : var(v), exp(e), left(NULL), right(NULL), header(NULL), tag(node)
  //  { val.down = d; }
  //
  //  Nmi_node(int v, int e, Bag *b)
  //    : var(v), exp(e), left(NULL), right(NULL), header(NULL), tag(leaf)
  //  { val.bag = b; }
  //
  //  ~Nmi_node();

  Nmi_node *&down() { return val.down; }
  Bag *&baggage() { return val.bag; }
  intarray &monom() { return val.bag->monom(); }              // varpower
  const intarray &monom() const { return val.bag->monom(); }  // varpower
  void insert_to_left(Nmi_node *q)
  {
    q->header = header;
    q->left = left;
    q->right = this;
    left = left->right = q;
  }
};

class MonomialIdeal : public EngineObject
// these objects are immutable once they are sent to the front end (i.e.
// once their hash code has been set...
{
  const PolynomialRing *R;
  Nmi_node *mi;
  int count;  // We hack this a bit: the low order bit (count%1==1) means that
              // we own the stash
              // count//2 is the actual number of nodes here.
  stash *mi_stash;
  friend class AssociatedPrimes;
  friend class MinimalPrimes;

 private:
  Nmi_node *new_mi_node(int v, int e, Nmi_node *d);
  Nmi_node *new_mi_node(int v, int e, Bag *b);
  void delete_mi_node(Nmi_node *p);

  Nmi_node *first_node() const;
  Nmi_node *last_node() const;
  Nmi_node *next(Nmi_node *p) const;
  Nmi_node *prev(Nmi_node *p) const;

  void insert1(Nmi_node *&p, Bag *b);
  void remove1(Nmi_node *p);
  void do_node(Nmi_node *p, int indent, int disp) const;
  void do_tree(Nmi_node *p, int depth, int indent, int disp) const;
  int debug_check(Nmi_node *p, const Nmi_node *up) const;

 protected:
  virtual unsigned int computeHashValue() const;

 public:
  MonomialIdeal(const PolynomialRing *RR, stash *mi_stash = 0);
  virtual ~MonomialIdeal() { remove_MonomialIdeal(); }
  int length_of() const { return count / 2; }
  MonomialIdeal(const PolynomialRing *R,
                queue<Bag *> &elems,
                stash *mi_stash = 0);
  MonomialIdeal(const PolynomialRing *R,
                queue<Bag *> &elems,
                queue<Bag *> &rejects,
                stash *mi_stash = 0);

  MonomialIdeal *copy() const;

  void remove_MonomialIdeal();     // frees all of the internal things
  const int *first_elem() const;   // returns varpower
  const int *second_elem() const;  // returns varpower

  // Informational
  int length() const { return count / 2; }
  int topvar() const { return (mi == NULL ? -1 : mi->var); }
  void text_out(buffer &o) const;

  const PolynomialRing *get_ring() const { return R; }
  const Monoid *degree_monoid() const
  {
    const Ring *S = R;
    return S->degree_monoid();
  }

  // Insertion of new monomials.
  void insert_minimal(Bag *b);
  // Insert baggage 'b'.  It is assumed
  // that 'b' is not already in the monomial ideal.

  int insert(Bag *b);

  void insert_w_deletions(Bag *b, queue<Bag *> &deletions);
  // Insert 'm', removing any monomials divisible by 'm', and
  // returning their baggage in a list of moninfo *'s.

  int remove(Bag *&b);
  // Deletion.  Remove the lexicographically largest element, placing
  // it into b.  Return 1 if an element is removed.

  int search_expvector(const int *m, Bag *&b) const;
  // Search.  Return whether a monomial which divides 'm' is
  // found.  If so, return the baggage.  'm' is assumed to be an
  // exponent vector of length larger than the top variable occuring
  // in 'this'
  int search(const int *m, Bag *&b) const;
  // Search.  Return whether a monomial which divides 'm' is
  // found.  If so, return the baggage.  'm' is a varpower monomial.
  void find_all_divisors(const int *exp, VECTOR(Bag *)& b) const;
  // Search. Return a list of all elements which divide 'exp'.

  Bag *operator[](Index<MonomialIdeal> i) const;
  Index<MonomialIdeal> first() const;
  Index<MonomialIdeal> last() const;

  void *next(void *p) const;
  void *prev(void *p) const;
  int valid(void *p) const { return (p != NULL); }
  void debug_out(int disp = 1) const;
  void debug_check() const;

  bool is_equal(const MonomialIdeal &mi) const;

  MonomialIdeal *intersect(const int *m) const;  // m is a varpower monomial
  MonomialIdeal *intersect(const MonomialIdeal &J) const;
  MonomialIdeal *quotient(const int *m) const;  // m is a varpower monomial
  MonomialIdeal *quotient(const MonomialIdeal &J) const;
  MonomialIdeal *erase(const int *m) const;  // m is a varpower monomial
  MonomialIdeal *sat(const MonomialIdeal &J) const;

  M2_arrayint lcm() const;
  // Returns the lcm of all of the generators of this, as an array of ints

  MonomialIdeal *alexander_dual(const M2_arrayint a) const;
  // a is a vector which is entrywise >= lcm(this).

  MonomialIdeal *radical() const;

  MonomialIdeal *borel() const;
  bool is_borel() const;

  MonomialIdeal *operator+(const MonomialIdeal &F) const;
  MonomialIdeal *operator-(const MonomialIdeal &F) const;
  MonomialIdeal *operator*(const MonomialIdeal &G) const;

  bool is_one() const;
  int n_pure_powers() const;
};

struct monideal_pair : public our_new_delete
{
  MonomialIdeal *mi;
  MonomialIdeal *mi_search;

  monideal_pair(const PolynomialRing *R)
      : mi(new MonomialIdeal(R)), mi_search(new MonomialIdeal(R))
  {
  }

  monideal_pair(const PolynomialRing *R, stash *mi_stash)
      : mi(new MonomialIdeal(R, mi_stash)),
        mi_search(new MonomialIdeal(R, mi_stash))
  {
  }
};

//-----------------------------------------------------------------

inline void MonomialIdeal::insert_minimal(Bag *b)
{
  insert1(mi, b);
  count += 2;
}

inline Bag *MonomialIdeal::operator[](Index<MonomialIdeal> i) const
{
  Nmi_node *p = reinterpret_cast<Nmi_node *>(i.val());
  return p->baggage();
}

inline Nmi_node *MonomialIdeal::first_node() const { return next(mi); }
inline Nmi_node *MonomialIdeal::last_node() const { return prev(mi); }
inline Index<MonomialIdeal> MonomialIdeal::first() const
{
  return Index<MonomialIdeal>(next(reinterpret_cast<void *>(mi)), this);
}

inline Index<MonomialIdeal> MonomialIdeal::last() const
{
  return Index<MonomialIdeal>(prev(reinterpret_cast<void *>(mi)), this);
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:

// TODO: move this file: these are interface routines

#ifndef __monordering_hpp_
#define __monordering_hpp_

#include <string>
#include <vector>

class MonomialOrderings {
public:
  static std::string toString(const MonomialOrdering *mo);
  static MonomialOrdering* join(const std::vector<MonomialOrdering*>& M);
  static MonomialOrdering* product(const std::vector<MonomialOrdering*>& M);
  static MonomialOrdering* Lex(int nvars);
  static MonomialOrdering* Lex2(int nvars);
  static MonomialOrdering* Lex4(int nvars);
  static MonomialOrdering* GRevLex(int nvars);
  static MonomialOrdering* GRevLex2(int nvars);
  static MonomialOrdering* GRevLex4(int nvars);
  static MonomialOrdering* GRevLex(const std::vector<int>& wts);
  static MonomialOrdering* GRevLex2(const std::vector<int>& wts);
  static MonomialOrdering* GRevLex4(const std::vector<int>& wts);
  static MonomialOrdering* RevLex(int nvars);
  static MonomialOrdering* Weights(const std::vector<int>& wts);
  static MonomialOrdering* GroupLex(int nvars);
  static MonomialOrdering* GroupRevLex(int nvars);
  static MonomialOrdering* PositionUp();
  static MonomialOrdering* PositionDown();

  static MonomialOrdering* GRevLex(const std::vector<int>& wts, int packing);
};

// This is currently located in interface/monomial-ordering.cpp
bool monomialOrderingToMatrix(
    const struct MonomialOrdering &mo,
    std::vector<int> &mat,
    bool &base_is_revlex,
    int &component_direction,      // -1 is Down, +1 is Up, 0 is not present
    int &component_is_before_row);  // -1 means at the end, 0 means before the
                                   // order, and r means considered before row
                                   // 'r' of the matrix.

#endif

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
*/

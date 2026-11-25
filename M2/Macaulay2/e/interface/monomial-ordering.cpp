// Copyright 2002 Michael E. Stillman

#include "interface/monomial-ordering.h"

#include <vector>

#include "monomials/monordering.hpp"
#include "util.hpp"

MonomialOrdering* rawLexMonomialOrdering(int nvars, int packing)
{
  if (packing == 2) return MonomialOrderings::Lex2(nvars);
  if (packing == 4) return MonomialOrderings::Lex4(nvars);
  return MonomialOrderings::Lex(nvars);
}

MonomialOrdering* rawGRevLexMonomialOrdering(M2_arrayint degrees, int packing)
{
  return MonomialOrderings::GRevLex(M2_arrayint_to_stdvector<int>(degrees), packing);
}

MonomialOrdering* rawRevLexMonomialOrdering(int nvars) { return MonomialOrderings::RevLex(nvars); }
MonomialOrdering* rawWeightsMonomialOrdering(M2_arrayint weights)
{
  return MonomialOrderings::Weights(M2_arrayint_to_stdvector<int>(weights));
}
MonomialOrdering* rawGroupLexMonomialOrdering(int nvars) { return MonomialOrderings::GroupLex(nvars); }
MonomialOrdering* rawGroupRevLexMonomialOrdering(int nvars) { return MonomialOrderings::GroupRevLex(nvars); }

MonomialOrdering* rawNClexMonomialOrdering(int nvars)
{
  return MonomialOrderings::NCLex(nvars);
}

MonomialOrdering* rawPositionMonomialOrdering(M2_bool upOrDown)
{
  return upOrDown ? MonomialOrderings::PositionUp() : MonomialOrderings::PositionDown();
}

static std::vector<MonomialOrdering*> toVector(engine_RawMonomialOrderingArray orderings)
{
  std::vector<MonomialOrdering*> result;
  result.reserve(orderings->len);
  for (int i = 0; i < orderings->len; ++i)
    result.push_back(const_cast<MonomialOrdering*>(orderings->array[i]));
  return result;
}

MonomialOrdering* rawProductMonomialOrdering(engine_RawMonomialOrderingArray orderings)
{
  return MonomialOrderings::product(toVector(orderings));
}

MonomialOrdering* rawJoinMonomialOrdering(engine_RawMonomialOrderingArray orderings)
{
  return MonomialOrderings::join(toVector(orderings));
}

int rawNumberOfVariables(const MonomialOrdering* mo) { return MonomialOrderings::numberOfVariables(mo); }
int rawNumberOfInvertibleVariables(const MonomialOrdering* mo)
{
  return MonomialOrderings::numberOfInvertibleVariables(mo);
}
M2_arrayint rawNonTermOrderVariables(const MonomialOrdering* mo)
{
  return stdvector_to_M2_arrayint(MonomialOrderings::nonTermOrderVariables(mo));
}
M2_string IM2_MonomialOrdering_to_string(const MonomialOrdering* mo)
{
  return string_std_to_M2(MonomialOrderings::toString(mo));
}
unsigned int rawMonomialOrderingHash(const MonomialOrdering* mo) { return MonomialOrderings::hash(mo); }
int moIsGRevLex(const MonomialOrdering* mo) { return MonomialOrderings::isGRevLex(mo); }
int moIsLex(const MonomialOrdering* mo) { return MonomialOrderings::isLex(mo); }
M2_arrayint moGetWeightValues(const MonomialOrdering* mo)
{
  if (mo->len == 0 || mo->array[0]->type != MO_WEIGHTS) return nullptr;
  return stdvector_to_M2_arrayint(MonomialOrderings::firstWeightVector(mo));
}

M2_arrayint rawMonomialOrderingToMatrix(const MonomialOrdering* mo)
{
  bool base;
  std::vector<int> matrix;
  int componentIsBeforeRow = 0;
  int componentDirection = 0;
  if (!monomialOrderingToMatrix(*mo, matrix, base, componentDirection, componentIsBeforeRow)) return nullptr;
  int top = static_cast<int>(matrix.size());
  M2_arrayint result = M2_makearrayint(top + 3);
  for (int i = 0; i < top; ++i) result->array[i] = matrix[i];
  result->array[top] = base ? 1 : 0;
  result->array[top + 1] = componentDirection;
  result->array[top + 2] = componentIsBeforeRow;
  return result;
}

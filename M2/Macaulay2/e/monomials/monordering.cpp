// Copyright 2002 Michael E. Stillman

#include "monomials/monordering.hpp"

#include <algorithm>
#include <sstream>

#include "error.h"
#include "interface/m2-mem.h"
#include "interface/monomial-ordering.h"

namespace {
mon_part makePart(enum MonomialOrdering_type type,
                  int nvars,
                  const int* weights)
{
  auto result = getmemstructtype(mon_part);
  result->type = type;
  result->nvars = nvars;
  if (weights == nullptr)
    result->wts = nullptr;
  else
    {
      result->wts = getmematomicvectortype(int, nvars);
      for (int i = 0; i < nvars; ++i) result->wts[i] = weights[i];
    }
  return result;
}

MonomialOrdering* makeOrdering(int length)
{
  static unsigned int nextHash = 23023421;
  auto result = getmemarraytype(MonomialOrdering*, length);
  result->len = length;
  result->_hash = nextHash++;
  for (int i = 0; i < length; ++i) result->array[i] = nullptr;
  return result;
}

MonomialOrdering* offset(const MonomialOrdering* mo, int amount)
{
  auto result = makeOrdering(mo->len);
  for (int i = 0; i < mo->len; ++i)
    {
      auto part = mo->array[i];
      if (part->type != MO_WEIGHTS)
        result->array[i] = makePart(part->type, part->nvars, part->wts);
      else
        {
          auto shifted = makePart(MO_WEIGHTS, amount + part->nvars, nullptr);
          shifted->wts = getmemvectortype(int, shifted->nvars);
          for (int j = 0; j < amount; ++j) shifted->wts[j] = 0;
          for (int j = amount; j < shifted->nvars; ++j)
            shifted->wts[j] = part->wts[j - amount];
          result->array[i] = shifted;
        }
    }
  return result;
}

bool isGood(mon_part part)
{
  switch (part->type)
    {
      case MO_LEX:
      case MO_LEX2:
      case MO_LEX4:
      case MO_GREVLEX:
      case MO_GREVLEX2:
      case MO_GREVLEX4:
      case MO_GREVLEX_WTS:
      case MO_GREVLEX2_WTS:
      case MO_GREVLEX4_WTS:
      case MO_LAURENT:
      case MO_NC_LEX:
      case MO_LAURENT_REVLEX:
      case MO_REVLEX:
      case MO_WEIGHTS:
        return part->nvars > 0;
      case MO_POSITION_UP:
      case MO_POSITION_DOWN:
        return true;
    }
  return false;
}

MonomialOrdering* makeSinglePart(enum MonomialOrdering_type type, int nvars)
{
  auto result = makeOrdering(1);
  result->array[0] = makePart(type, nvars, nullptr);
  return result;
}

std::ostringstream& writeArray(std::ostringstream& output, int length, int* values)
{
  output << "{";
  for (int i = 0; i < length; ++i)
    {
      if (i != 0) output << ",";
      output << values[i];
    }
  output << "}";
  return output;
}

std::ostringstream& writeOnes(std::ostringstream& output, int length)
{
  output << "{";
  for (int i = 0; i < length; ++i)
    {
      if (i != 0) output << ",";
      output << 1;
    }
  output << "}";
  return output;
}
}  // namespace

MonomialOrdering* MonomialOrderings::Lex(int nvars) { return makeSinglePart(MO_LEX, nvars); }
MonomialOrdering* MonomialOrderings::Lex2(int nvars) { return makeSinglePart(MO_LEX2, nvars); }
MonomialOrdering* MonomialOrderings::Lex4(int nvars) { return makeSinglePart(MO_LEX4, nvars); }
MonomialOrdering* MonomialOrderings::GRevLex(int nvars) { return GRevLex(std::vector<int>(nvars, 1)); }
MonomialOrdering* MonomialOrderings::GRevLex2(int nvars) { return GRevLex2(std::vector<int>(nvars, 1)); }
MonomialOrdering* MonomialOrderings::GRevLex4(int nvars) { return GRevLex4(std::vector<int>(nvars, 1)); }
MonomialOrdering* MonomialOrderings::GRevLex(const std::vector<int>& weights) { return GRevLex(weights, 1); }
MonomialOrdering* MonomialOrderings::GRevLex2(const std::vector<int>& weights) { return GRevLex(weights, 2); }
MonomialOrdering* MonomialOrderings::GRevLex4(const std::vector<int>& weights) { return GRevLex(weights, 4); }
MonomialOrdering* MonomialOrderings::RevLex(int nvars) { return makeSinglePart(MO_REVLEX, nvars); }

MonomialOrdering* MonomialOrderings::Weights(const std::vector<int>& weights)
{
  auto result = makeOrdering(1);
  result->array[0] = makePart(MO_WEIGHTS, weights.size(), weights.data());
  return result;
}

MonomialOrdering* MonomialOrderings::GroupLex(int nvars) { return makeSinglePart(MO_LAURENT, nvars); }
MonomialOrdering* MonomialOrderings::GroupRevLex(int nvars) { return makeSinglePart(MO_LAURENT_REVLEX, nvars); }
MonomialOrdering* MonomialOrderings::NCLex(int nvars) { return makeSinglePart(MO_NC_LEX, nvars); }
MonomialOrdering* MonomialOrderings::PositionUp() { return makeSinglePart(MO_POSITION_UP, 0); }
MonomialOrdering* MonomialOrderings::PositionDown() { return makeSinglePart(MO_POSITION_DOWN, 0); }

MonomialOrdering* MonomialOrderings::GRevLex(const std::vector<int>& degrees, int packing)
{
  bool allOne = true;
  for (auto degree : degrees)
    if (degree <= 0)
      {
        ERROR("grevlex: expected all degrees to be positive");
        return nullptr;
      }
    else if (degree > 1)
      allOne = false;

  enum MonomialOrdering_type type;
  if (allOne)
    {
      if (packing == 2)
        type = MO_GREVLEX2;
      else if (packing == 4)
        type = MO_GREVLEX4;
      else
        type = MO_GREVLEX;
    }
  else
    {
      if (packing == 2)
        type = MO_GREVLEX2_WTS;
      else if (packing == 4)
        type = MO_GREVLEX4_WTS;
      else
        type = MO_GREVLEX_WTS;
    }
  auto result = makeOrdering(1);
  result->array[0] = makePart(type, degrees.size(), allOne ? nullptr : degrees.data());
  return result;
}

MonomialOrdering* MonomialOrderings::join(const std::vector<MonomialOrdering*>& orderings)
{
  int length = 0;
  for (auto ordering : orderings)
    for (int j = 0; j < ordering->len; ++j)
      if (isGood(ordering->array[j])) ++length;

  auto result = makeOrdering(length);
  int next = 0;
  int variablesSoFar = 0;
  for (auto ordering : orderings)
    for (int j = 0; j < ordering->len; ++j)
      {
        auto part = ordering->array[j];
        if (!isGood(part)) continue;
        if (part->type != MO_WEIGHTS)
          variablesSoFar += part->nvars;
        else
          {
            auto shifted = makePart(MO_WEIGHTS, variablesSoFar + part->nvars, nullptr);
            shifted->wts = getmemvectortype(int, shifted->nvars);
            for (int k = 0; k < variablesSoFar; ++k) shifted->wts[k] = 0;
            for (int k = variablesSoFar; k < shifted->nvars; ++k)
              shifted->wts[k] = part->wts[k - variablesSoFar];
            part = shifted;
          }
        result->array[next++] = part;
      }
  return result;
}

MonomialOrdering* MonomialOrderings::product(const std::vector<MonomialOrdering*>& orderings)
{
  int length = 0;
  for (auto ordering : orderings) length += ordering->len;
  auto result = makeOrdering(length);
  int next = 0;
  int amount = 0;
  for (auto ordering : orderings)
    {
      auto shifted = offset(ordering, amount);
      for (int j = 0; j < shifted->len; ++j) result->array[next++] = shifted->array[j];
      amount += numberOfVariables(ordering);
    }
  return result;
}

std::string MonomialOrderings::toString(const MonomialOrdering* mo)
{
  std::ostringstream output;
  output << "MonomialOrder => {";
  for (int i = 0; i < mo->len; ++i)
    {
      auto part = mo->array[i];
      bool ones = false;
      output << (i == 0 ? "\n    " : ",\n    ");
      switch (part->type)
        {
          case MO_LEX: output << "Lex => " << part->nvars; break;
          case MO_LEX2: output << "LexSmall => " << part->nvars; break;
          case MO_LEX4: output << "LexTiny => " << part->nvars; break;
          case MO_GREVLEX: output << "GRevLex => "; ones = true; break;
          case MO_GREVLEX2: output << "GRevLexSmall => "; ones = true; break;
          case MO_GREVLEX4: output << "GRevLexTiny => "; ones = true; break;
          case MO_GREVLEX_WTS: output << "GRevLex => "; break;
          case MO_GREVLEX2_WTS: output << "GRevLexSmall => "; break;
          case MO_GREVLEX4_WTS: output << "GRevLexTiny => "; break;
          case MO_REVLEX: output << "RevLex => " << part->nvars; break;
          case MO_WEIGHTS: output << "Weights => "; break;
          case MO_LAURENT: output << "GroupLex => " << part->nvars; break;
          case MO_LAURENT_REVLEX: output << "GroupRevLex => " << part->nvars; break;
          case MO_NC_LEX: output << "NCLex => " << part->nvars; break;
          case MO_POSITION_UP: output << "Position => Up"; break;
          case MO_POSITION_DOWN: output << "Position => Down"; break;
          default: output << "UNKNOWN"; break;
        }
      if (part->wts != nullptr) writeArray(output, part->nvars, part->wts);
      else if (ones) writeOnes(output, part->nvars);
    }
  output << "\n    }";
  return output.str();
}

bool MonomialOrderings::isLex(const MonomialOrdering* mo)
{
  // The monomial order is lex if what?
  // one lex block, no grevlex blocks, no weightvector blocks.
  // only: lex block and position blocks are allowed.
  int blocks = 0;
  for (int i = 0; i < mo->len; ++i)
    switch (mo->array[i]->type)
      {
        case MO_LEX: case MO_LEX2: case MO_LEX4: ++blocks; break;
        case MO_POSITION_UP: case MO_POSITION_DOWN: break;
        default: return false;
      }
  return blocks == 1;
}

bool MonomialOrderings::isGRevLex(const MonomialOrdering* mo)
{
  int blocks = 0;
  for (int i = 0; i < mo->len; ++i)
    switch (mo->array[i]->type)
      {
        case MO_GREVLEX: case MO_GREVLEX2: case MO_GREVLEX4:
        case MO_GREVLEX_WTS: case MO_GREVLEX2_WTS: case MO_GREVLEX4_WTS:
          ++blocks; break;
        case MO_POSITION_UP: case MO_POSITION_DOWN: break;
        default: return false;
      }
  return blocks == 1;
}

int MonomialOrderings::numberOfVariables(const MonomialOrdering* mo)
{
  int result = 0;
  for (int i = 0; i < mo->len; ++i)
    if (mo->array[i]->type != MO_WEIGHTS) result += mo->array[i]->nvars;
  return result;
}

int MonomialOrderings::numberOfInvertibleVariables(const MonomialOrdering* mo)
{
  int result = 0;
  for (int i = 0; i < mo->len; ++i)
    if (mo->array[i]->type == MO_LAURENT || mo->array[i]->type == MO_LAURENT_REVLEX)
      result += mo->array[i]->nvars;
  return result;
}

std::vector<int> MonomialOrderings::firstWeightVector(const MonomialOrdering* mo)
{
  if (mo->len == 0 || mo->array[0]->type != MO_WEIGHTS) return {};
  // nvars is the length of the weight vector here, not a variable count
  auto result = std::vector<int>(mo->array[0]->wts,
                                 mo->array[0]->wts + mo->array[0]->nvars);
  result.resize(numberOfVariables(mo), 0);
  return result;
}

std::vector<int> MonomialOrderings::nonTermOrderVariables(const MonomialOrdering* mo)
{
  auto relation = std::vector<int>(numberOfVariables(mo), 0);
  int next = 0;
  for (int i = 0; i < mo->len; ++i)
    {
      auto part = mo->array[i];
      switch (part->type)
        {
          case MO_LEX: case MO_LEX2: case MO_LEX4: case MO_GREVLEX:
          case MO_GREVLEX2: case MO_GREVLEX4: case MO_GREVLEX_WTS:
          case MO_GREVLEX2_WTS: case MO_GREVLEX4_WTS: case MO_LAURENT:
          case MO_NC_LEX:
            for (int j = 0; j < part->nvars; ++j, ++next)
              if (relation[next] == 0) relation[next] = 1;
            break;
          case MO_LAURENT_REVLEX: case MO_REVLEX:
            for (int j = 0; j < part->nvars; ++j, ++next)
              if (relation[next] == 0) relation[next] = -1;
            break;
          case MO_WEIGHTS:
            {
              int top = std::min(part->nvars, static_cast<int>(relation.size()));
              for (int j = next; j < top; ++j)
                if (relation[j] == 0)
                  {
                    if (part->wts[j] > 0)
                      relation[j] = 1;
                    else if (part->wts[j] < 0)
                      relation[j] = -1;
                  }
            }
            break;
          case MO_POSITION_UP: case MO_POSITION_DOWN: break;
        }
    }
  std::vector<int> result;
  for (int i = 0; i < relation.size(); ++i)
    {
      if (relation[i] == 0) INTERNAL_ERROR("relation[i] should not be 0");
      if (relation[i] < 0) result.push_back(i);
    }
  return result;
}

unsigned int MonomialOrderings::hash(const MonomialOrdering* mo) { return mo->_hash; }

namespace {
void writeRow(std::vector<int>& grading, int nvars, int which, int value)
{
  for (int i = 0; i < nvars; ++i) grading.push_back(i == which ? value : 0);
}

void writeWeights(std::vector<int>& grading,
                  int nvars,
                  int firstvar,
                  int* weights,
                  int numberOfWeights)
{
  for (int i = 0; i < firstvar; ++i) grading.push_back(0);
  if (weights == nullptr)
    for (int i = 0; i < numberOfWeights; ++i) grading.push_back(1);
  else
    for (int i = 0; i < numberOfWeights; ++i) grading.push_back(weights[i]);
  for (int i = firstvar + numberOfWeights; i < nvars; ++i) grading.push_back(0);
}
}  // namespace

bool monomialOrderingToMatrix(const MonomialOrdering& mo,
                              std::vector<int>& matrix,
                              bool& baseIsRevLex,
                              int& componentDirection,
                              int& componentIsBeforeRow)
{
  int nvars = MonomialOrderings::numberOfVariables(&mo);
  baseIsRevLex = true;
  enum LastBlock { LEX, REVLEX, WEIGHTS, NONE };
  LastBlock last = NONE;
  int rows = 0;
  int firstvar = 0;
  componentDirection = 0;
  componentIsBeforeRow = -2;
  size_t lastElement = 0;
  for (int i = 0; i < mo.len; ++i)
    {
      mon_part part = mo.array[i];
      switch (part->type)
        {
          case MO_LEX: case MO_LEX2: case MO_LEX4:
            lastElement = matrix.size();
            for (int j = 0; j < part->nvars; ++j) writeRow(matrix, nvars, firstvar + j, 1);
            last = LEX;
            firstvar += part->nvars;
            rows += part->nvars;
            break;
          case MO_GREVLEX: case MO_GREVLEX2: case MO_GREVLEX4:
          case MO_GREVLEX_WTS: case MO_GREVLEX2_WTS: case MO_GREVLEX4_WTS:
            writeWeights(matrix, nvars, firstvar, part->wts, part->nvars);
            lastElement = matrix.size();
            for (int j = part->nvars - 1; j >= 1; --j) writeRow(matrix, nvars, firstvar + j, -1);
            last = REVLEX;
            firstvar += part->nvars;
            rows += part->nvars;
            break;
          case MO_REVLEX:
            lastElement = matrix.size();
            for (int j = part->nvars - 1; j >= 0; --j) writeRow(matrix, nvars, firstvar + j, -1);
            last = REVLEX;
            firstvar += part->nvars;
            rows += part->nvars;
            break;
          case MO_WEIGHTS:
            writeWeights(matrix, nvars, 0, part->wts, part->nvars > nvars ? nvars : part->nvars);
            ++rows;
            lastElement = matrix.size();
            last = WEIGHTS;
            break;
          case MO_LAURENT: case MO_LAURENT_REVLEX: case MO_NC_LEX:
            return false;
          case MO_POSITION_UP:
            componentDirection = 1;
            componentIsBeforeRow = rows;
            break;
          case MO_POSITION_DOWN:
            componentDirection = -1;
            componentIsBeforeRow = rows;
            break;
          default: break;
        }
    }
  if (last == LEX)
    {
      // last block was lex, so use lex tie-breaker
      matrix.resize(lastElement);
      if (rows == componentIsBeforeRow) componentIsBeforeRow = -1;
      baseIsRevLex = false;
    }
  else if (last == REVLEX)
    {
      // last block was revlex, so use revlex tie-breaker
      if (rows == componentIsBeforeRow) componentIsBeforeRow = -1;
      matrix.resize(lastElement);
    }
  // last block is a weight vector, so use revlex as the tie-breaker.
  // nothing to change here.
  return true;
}

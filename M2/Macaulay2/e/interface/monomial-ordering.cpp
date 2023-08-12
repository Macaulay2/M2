// TODO: there are many duplicate methods in C and C++ style in this file

#include "interface/monomial-ordering.h"

// TODO: remove this when to_string methods are moved together
#include <stdio.h>

#include <vector>
#include <string>
#include <sstream>

#include "error.h"
#include "monordering.hpp" // TODO: where can this go? it only defines one class

static struct mon_part_rec_ *mo_make(enum MonomialOrdering_type type,
                                     int nvars,
                                     const int *wts)
{
  mon_part result;
  result = getmemstructtype(mon_part);
  result->type = type;
  result->nvars = nvars;
  if (wts != nullptr)
    {
      int i;
      result->wts = getmematomicvectortype(int, nvars);
      for (i = 0; i < nvars; i++) result->wts[i] = wts[i];
    }
  else
    result->wts = nullptr;
  return result;
}

static MonomialOrdering *make_mon_order(int n)
{
  static unsigned int next_hash = 23023421;
  MonomialOrdering *z = getmemarraytype(MonomialOrdering *, n);
  z->len = n;
  z->_hash = next_hash++;
  int i;
  for (i = 0; i < n; i++) z->array[i] = nullptr;
  return z;
}

static MonomialOrdering *M2_mo_offset(const MonomialOrdering *mo, int offset)
{
  int i, j;
  MonomialOrdering *result = make_mon_order(mo->len);
  for (i = 0; i < mo->len; i++)
    {
      mon_part p = mo->array[i];
      if (p->type != MO_WEIGHTS)
        result->array[i] = mo_make(p->type, p->nvars, p->wts);
      else
        {
          mon_part q = mo_make(MO_WEIGHTS, offset + p->nvars, nullptr);
          q->wts = getmemvectortype(int, q->nvars);
          for (j = 0; j < offset; j++) q->wts[j] = 0;
          for (; j < q->nvars; j++) q->wts[j] = p->wts[j - offset];
        }
    }
  return result;
}

static bool is_good(mon_part p)
{
  switch (p->type)
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
        return (p->nvars > 0);
      case MO_POSITION_UP:
      case MO_POSITION_DOWN:
        return true;
    }
  return false;
}

MonomialOrdering *MonomialOrderings::Lex(int nvars)
{
  return rawLexMonomialOrdering(nvars, 1);
}
MonomialOrdering *MonomialOrderings::Lex2(int nvars)
{
  return rawLexMonomialOrdering(nvars, 2);
}
MonomialOrdering *MonomialOrderings::Lex4(int nvars)
{
  return rawLexMonomialOrdering(nvars, 4);
}
MonomialOrdering *MonomialOrderings::GRevLex(int nvars)
{
  std::vector<int> w;
  for (int i = 0; i < nvars; i++) w.push_back(1);
  return GRevLex(w);
}
MonomialOrdering *MonomialOrderings::GRevLex2(int nvars)
{
  std::vector<int> w;
  for (int i = 0; i < nvars; i++) w.push_back(1);
  return GRevLex2(w);
}
MonomialOrdering *MonomialOrderings::GRevLex4(int nvars)
{
  std::vector<int> w;
  for (int i = 0; i < nvars; i++) w.push_back(1);
  return GRevLex4(w);
}
MonomialOrdering *MonomialOrderings::GRevLex(const std::vector<int> &wts)
{
  return GRevLex(wts, 1);
}
MonomialOrdering *MonomialOrderings::GRevLex2(const std::vector<int> &wts)
{
  return GRevLex(wts, 2);
}
MonomialOrdering *MonomialOrderings::GRevLex4(const std::vector<int> &wts)
{
  return GRevLex(wts, 4);
}
MonomialOrdering *MonomialOrderings::RevLex(int nvars)
{
  return rawRevLexMonomialOrdering(nvars);
}
MonomialOrdering *MonomialOrderings::Weights(const std::vector<int> &wts)
{
  mon_part p = mo_make(MO_WEIGHTS, wts.size(), wts.data());
  MonomialOrdering *result = make_mon_order(1);
  result->array[0] = p;
  return result;
}
MonomialOrdering *MonomialOrderings::GroupLex(int nvars)
{
  return rawGroupLexMonomialOrdering(nvars);
}
MonomialOrdering *MonomialOrderings::GroupRevLex(int nvars)
{
  return rawGroupRevLexMonomialOrdering(nvars);
}
MonomialOrdering *MonomialOrderings::PositionUp()
{
  return rawPositionMonomialOrdering(true);
}
MonomialOrdering *MonomialOrderings::PositionDown()
{
  return rawPositionMonomialOrdering(false);
}

MonomialOrdering *MonomialOrderings::GRevLex(const std::vector<int> &degs,
                                             int packing)
{
  MonomialOrdering *result;
  mon_part p;
  enum MonomialOrdering_type typ;
  const int *wts;
  bool all_one = true;
  for (int i = 0; i < degs.size(); i++)
    if (degs[i] <= 0)
      {
        ERROR("grevlex: expected all degrees to be positive");
        return nullptr;
      }
    else if (degs[i] > 1)
      all_one = false;

  if (all_one)
    {
      if (packing == 2)
        typ = MO_GREVLEX2;
      else if (packing == 4)
        typ = MO_GREVLEX4;
      else
        typ = MO_GREVLEX;
      wts = nullptr;
    }
  else
    {
      if (packing == 2)
        typ = MO_GREVLEX2_WTS;
      else if (packing == 4)
        typ = MO_GREVLEX4_WTS;
      else
        typ = MO_GREVLEX_WTS;
      wts = degs.data();
    }

  p = mo_make(typ, degs.size(), wts);
  result = make_mon_order(1);
  result->array[0] = p;
  return result;
}

MonomialOrdering *MonomialOrderings::join(
    const std::vector<MonomialOrdering *> &M)
{
  MonomialOrdering *result;
  const MonomialOrdering *mo;
  int i, j, sum, next;
  int nvars_so_far = 0;

  sum = 0;
  for (i = 0; i < M.size(); i++)
    {
      mo = M[i];
      for (j = 0; j < mo->len; j++)
        if (is_good(mo->array[j])) sum++;
    }

  result = make_mon_order(sum);
  next = 0;
  for (i = 0; i < M.size(); i++)
    {
      mo = M[i];
      for (j = 0; j < mo->len; j++)
        {
          mon_part p = mo->array[j];
          if (!is_good(p)) continue;
          if (p->type != MO_WEIGHTS)
            nvars_so_far += p->nvars;
          else
            {
              /* Shift the weights over by nvars_so_far */
              mon_part q = mo_make(MO_WEIGHTS, nvars_so_far + p->nvars, nullptr);
              q->wts = getmemvectortype(int, q->nvars);
              for (j = 0; j < nvars_so_far; j++) q->wts[j] = 0;
              for (; j < q->nvars; j++) q->wts[j] = p->wts[j - nvars_so_far];
              p = q;
            }
          result->array[next++] = p;
        }
    }
  return result;
}

MonomialOrdering *MonomialOrderings::product(
    const std::vector<MonomialOrdering *> &M)
{
  MonomialOrdering *result;
  int i, j, sum, next, offset;
  sum = 0;
  for (i = 0; i < M.size(); i++) sum += M[i]->len;

  result = make_mon_order(sum);
  next = 0;
  offset = 0;
  for (i = 0; i < M.size(); i++)
    {
      int nvars = rawNumberOfVariables(M[i]);
      MonomialOrdering *mo = M2_mo_offset(M[i], offset);
      for (j = 0; j < mo->len; j++) result->array[next++] = mo->array[j];
      offset += nvars;
    }
  return result;
}

std::ostringstream &toString(std::ostringstream &o, int len, int *p)
{
  o << "{";
  for (int i = 0; i < len; i++)
    {
      if (i > 0) o << ",";
      o << p[i];
    }
  o << "}";
  return o;
}

std::ostringstream &ones(std::ostringstream &o, int len)
{
  o << "{";
  for (int i = 0; i < len; i++)
    {
      if (i > 0) o << ",";
      o << 1;
    }
  o << "}";
  return o;
}

std::string MonomialOrderings::toString(const MonomialOrdering *mo)
{
  std::ostringstream o;
  o << "MonomialOrder => {";
  for (int i = 0; i < mo->len; i++)
    {
      mon_part p = mo->array[i];
      bool p_ones = false;
      if (i == 0)
        o << "\n    ";
      else
        o << ",\n    ";
      switch (p->type)
        {
          case MO_LEX:
            o << "Lex => " << p->nvars;
            break;
          case MO_LEX2:
            o << "LexSmall => " << p->nvars;
            break;
          case MO_LEX4:
            o << "LexTiny => " << p->nvars;
            break;
          case MO_GREVLEX:
            o << "GRevLex => ";
            p_ones = true;
            break;
          case MO_GREVLEX2:
            o << "GRevLexSmall => ";
            p_ones = true;
            break;
          case MO_GREVLEX4:
            o << "GRevLexTiny => ";
            p_ones = true;
            break;
          case MO_GREVLEX_WTS:
            o << "GRevLex => ";
            break;
          case MO_GREVLEX2_WTS:
            o << "GRevLexSmall => ";
            break;
          case MO_GREVLEX4_WTS:
            o << "GRevLexTiny => ";
            break;
          case MO_REVLEX:
            o << "RevLex => " << p->nvars;
            break;
          case MO_WEIGHTS:
            o << "Weights => ";
            break;
          case MO_LAURENT:
            o << "GroupLex => " << p->nvars;
            break;
          case MO_LAURENT_REVLEX:
            o << "GroupRevLex => " << p->nvars;
            break;
          case MO_NC_LEX:
            o << "NCLex => " << p->nvars;
            break;
          case MO_POSITION_UP:
            o << "Position => Up";
            break;
          case MO_POSITION_DOWN:
            o << "Position => Down";
            break;
          default:
            o << "UNKNOWN";
            break;
        }
      if (p->wts != nullptr) { ::toString(o, p->nvars, p->wts); }
      else if (p_ones)
        {
          ::ones(o, p->nvars);
        }
    }
  o << "\n    }";
  return o.str();
}

///////// Below this is from monordering.h ////////////////

int moIsLex(const MonomialOrdering *mo)
{
  // The monomial order is lex if what?
  // one lex block, no grevlex blocks, no weightvector blocks.
  // only: lex block and position blocks are allowed.
  int nlex = 0;
  int i;
  for (i = 0; i < mo->len; i++)
    {
      enum MonomialOrdering_type typ = mo->array[i]->type;
      switch (typ)
        {
          case MO_LEX:
          case MO_LEX2:
          case MO_LEX4:
            nlex++;
            break;
          case MO_POSITION_UP:
          case MO_POSITION_DOWN:
            break;
          default:
            return 0;
        }
    }
  return (nlex == 1);
}

int moIsGRevLex(const MonomialOrdering *mo)
{
  // The monomial order is lex if what?
  // one lex block, no grevlex blocks, no weightvector blocks.
  // only: lex block and position blocks are allowed.
  int ngrevlex = 0;
  int i;
  for (i = 0; i < mo->len; i++)
    {
      enum MonomialOrdering_type typ = mo->array[i]->type;
      switch (typ)
        {
          case MO_GREVLEX:
          case MO_GREVLEX2:
          case MO_GREVLEX4:
          case MO_GREVLEX_WTS:
          case MO_GREVLEX2_WTS:
          case MO_GREVLEX4_WTS:
            ngrevlex++;
            break;
          case MO_POSITION_UP:
          case MO_POSITION_DOWN:
            break;
          default:
            return 0;
        }
    }
  return (ngrevlex == 1);
}

int rawNumberOfVariables(const MonomialOrdering *mo)
{
  int i, sum = 0;
  for (i = 0; i < mo->len; i++)
    if (mo->array[i]->type != MO_WEIGHTS) sum += mo->array[i]->nvars;
  return sum;
}

M2_arrayint moGetWeightValues(const MonomialOrdering *mo)
{
  int nvars = rawNumberOfVariables(mo);
  // grab the first weight vector
  if (mo->len == 0) return nullptr;
  if (mo->array[0]->type == MO_WEIGHTS)
    {
      int i;
      M2_arrayint result = M2_makearrayint(nvars);
      int *wts = mo->array[0]->wts;
      for (i = 0; i < mo->array[0]->nvars; i++) result->array[i] = wts[i];
      for (; i < nvars; i++) result->array[i] = 0;
      return result;
    }
  return nullptr;
}

int rawNumberOfInvertibleVariables(const MonomialOrdering *mo)
{
  int i, sum = 0;
  for (i = 0; i < mo->len; i++)
    if (mo->array[i]->type == MO_LAURENT ||
        mo->array[i]->type == MO_LAURENT_REVLEX)
      sum += mo->array[i]->nvars;
  return sum;
}

M2_arrayint rawNonTermOrderVariables(const MonomialOrdering *mo)
// returns a list of the indices of those variables which are less than 1 in
// the given monomial order.
{
  int i, j, sum, nextvar;
  int nvars = rawNumberOfVariables(mo);
  int *gt = getmematomicvectortype(int, nvars);
  for (i = 0; i < nvars; i++)
    gt[i] =
        0;  // 0 means undecided, -1 means non term order, 1 means term order
  // Now we loop through the parts of the monomial order
  nextvar = 0;
  for (i = 0; i < mo->len; i++)
    {
      mon_part p = mo->array[i];
      switch (p->type)
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
            for (j = 0; j < p->nvars; j++, nextvar++)
              if (gt[nextvar] == 0) gt[nextvar] = 1;
            break;
          case MO_LAURENT_REVLEX:
          case MO_REVLEX:
            for (j = 0; j < p->nvars; j++, nextvar++)
              if (gt[nextvar] == 0) gt[nextvar] = -1;
            break;
          case MO_WEIGHTS:
            for (j = nextvar; j < p->nvars; j++)
              if (gt[j] == 0)
                {
                  if (p->wts[j] > 0)
                    gt[j] = 1;
                  else if (p->wts[j] < 0)
                    gt[j] = -1;
                }
            break;
          case MO_POSITION_UP:
          case MO_POSITION_DOWN:
            break;
        }
    }
  // At this point every variables' gt should be 1 or -1.
  sum = 0;
  for (i = 0; i < nvars; i++)
    {
      if (gt[i] == 0) INTERNAL_ERROR("gt[i] should not be 0");
      if (gt[i] < 0) sum++;
    }

  // Make an array of this length.
  M2_arrayint result = M2_makearrayint(sum);
  nextvar = 0;
  for (i = 0; i < nvars; i++)
    if (gt[i] < 0) result->array[nextvar++] = i;
  return result;
}

MonomialOrdering *rawLexMonomialOrdering(int nvars, int packing)
{
  MonomialOrdering *result;
  mon_part p;
  enum MonomialOrdering_type typ;

  if (packing == 2)
    typ = MO_LEX2;
  else if (packing == 4)
    typ = MO_LEX4;
  else
    typ = MO_LEX;

  p = mo_make(typ, nvars, nullptr);
  result = make_mon_order(1);
  result->array[0] = p;
  return result;
}

MonomialOrdering /* or null */ *rawGRevLexMonomialOrdering(M2_arrayint degs,
                                                           int packing)
{
  MonomialOrdering *result;
  mon_part p;
  enum MonomialOrdering_type typ;
  int *wts;
  int all_one = 1;
  unsigned int i;
  for (i = 0; i < degs->len; i++)
    if (degs->array[i] <= 0)
      {
        ERROR("grevlex: expected all degrees to be positive");
        return nullptr;
      }
    else if (degs->array[i] > 1)
      all_one = 0;

  if (all_one)
    {
      if (packing == 2)
        typ = MO_GREVLEX2;
      else if (packing == 4)
        typ = MO_GREVLEX4;
      else
        typ = MO_GREVLEX;
      wts = nullptr;
    }
  else
    {
      if (packing == 2)
        typ = MO_GREVLEX2_WTS;
      else if (packing == 4)
        typ = MO_GREVLEX4_WTS;
      else
        typ = MO_GREVLEX_WTS;
      wts = degs->array;
    }

  p = mo_make(typ, degs->len, wts);
  result = make_mon_order(1);
  result->array[0] = p;
  return result;
}

MonomialOrdering *rawRevLexMonomialOrdering(int nvars)
{
  mon_part p = mo_make(MO_REVLEX, nvars, nullptr);
  MonomialOrdering *result = make_mon_order(1);
  result->array[0] = p;
  return result;
}

MonomialOrdering *rawWeightsMonomialOrdering(M2_arrayint wts)
{
  mon_part p = mo_make(MO_WEIGHTS, wts->len, wts->array);
  MonomialOrdering *result = make_mon_order(1);
  result->array[0] = p;
  return result;
}
MonomialOrdering *rawGroupLexMonomialOrdering(int nvars)
{
  mon_part p = mo_make(MO_LAURENT, nvars, nullptr);
  MonomialOrdering *result = make_mon_order(1);
  result->array[0] = p;
  return result;
}
MonomialOrdering *rawGroupRevLexMonomialOrdering(int nvars)
{
  mon_part p = mo_make(MO_LAURENT_REVLEX, nvars, nullptr);
  MonomialOrdering *result = make_mon_order(1);
  result->array[0] = p;
  return result;
}
MonomialOrdering *rawNClexMonomialOrdering(int nvars)
{
  mon_part p = mo_make(MO_NC_LEX, nvars, nullptr);
  MonomialOrdering *result = make_mon_order(1);
  result->array[0] = p;
  return result;
}
MonomialOrdering *rawPositionMonomialOrdering(M2_bool up_or_down)
{
  mon_part p =
      mo_make((up_or_down ? MO_POSITION_UP : MO_POSITION_DOWN), 0, nullptr);
  MonomialOrdering *result = make_mon_order(1);
  result->array[0] = p;
  return result;
}

MonomialOrdering *rawJoinMonomialOrdering(engine_RawMonomialOrderingArray M)
{
  MonomialOrdering *result;
  const MonomialOrdering *mo;
  int i, j, sum, next;
  int nvars_so_far = 0;

  //  sum = 0;
  //  for (i=0; i<M->len; i++)
  //    sum += M->array[i]->len;

  sum = 0;
  for (i = 0; i < M->len; i++)
    {
      mo = M->array[i];
      for (j = 0; j < mo->len; j++)
        if (is_good(mo->array[j])) sum++;
    }

  result = make_mon_order(sum);
  next = 0;
  for (i = 0; i < M->len; i++)
    {
      mo = M->array[i];
      for (j = 0; j < mo->len; j++)
        {
          mon_part p = mo->array[j];
          if (!is_good(p)) continue;
          if (p->type != MO_WEIGHTS)
            nvars_so_far += p->nvars;
          else
            {
              /* Shift the weights over by nvars_so_far */
              mon_part q = mo_make(MO_WEIGHTS, nvars_so_far + p->nvars, nullptr);
              q->wts = getmemvectortype(int, q->nvars);
              for (j = 0; j < nvars_so_far; j++) q->wts[j] = 0;
              for (; j < q->nvars; j++) q->wts[j] = p->wts[j - nvars_so_far];
              p = q;
            }
          result->array[next++] = p;
        }
    }
  return result;
}

MonomialOrdering *rawProductMonomialOrdering(engine_RawMonomialOrderingArray M)
{
  MonomialOrdering *result;
  int i, j, sum, next, offset;
  sum = 0;
  for (i = 0; i < M->len; i++) sum += M->array[i]->len;

  result = make_mon_order(sum);
  next = 0;
  offset = 0;
  for (i = 0; i < M->len; i++)
    {
      int nvars = rawNumberOfVariables(M->array[i]);
      MonomialOrdering *mo = M2_mo_offset(M->array[i], offset);
      for (j = 0; j < mo->len; j++) result->array[next++] = mo->array[j];
      offset += nvars;
    }
  return result;
}

M2_string intarray_to_string(int len, int *p)
{
  int i;
  const int N = 200;
  char s[N];
  M2_string result = M2_tostring("{");
  for (i = 0; i < len; i++)
    {
      if (i > 0) result = M2_join(result, M2_tostring(","));
      snprintf(s, N, "%d", p[i]);
      result = M2_join(result, M2_tostring(s));
    }
  result = M2_join(result, M2_tostring("}"));
  return result;
}

M2_string ones_to_string(int len)
{
  int i;
  const int N = 200;
  char s[N];
  M2_string one;
  M2_string result = M2_tostring("{");
  snprintf(s, N, "1");
  one = M2_tostring(s);
  for (i = 0; i < len; i++)
    {
      if (i > 0) result = M2_join(result, M2_tostring(","));
      result = M2_join(result, one);
    }
  result = M2_join(result, M2_tostring("}"));
  return result;
}

unsigned int rawMonomialOrderingHash(const MonomialOrdering *mo)
{
  return mo->_hash;
}

M2_string IM2_MonomialOrdering_to_string(const MonomialOrdering *mo)
{
  int i;
  const int N = 200;
  char s[N];
  int p_ones = 0;
  M2_string result = M2_tostring("MonomialOrder => {");
  for (i = 0; i < mo->len; i++)
    {
      mon_part p = mo->array[i];
      p_ones = 0;
      if (i == 0)
        result = M2_join(result, M2_tostring("\n    "));
      else
        result = M2_join(result, M2_tostring(",\n    "));
      switch (p->type)
        {
          case MO_LEX:
            snprintf(s, N, "Lex => %d", p->nvars);
            break;
          case MO_LEX2:
            snprintf(s, N, "LexSmall => %d", p->nvars);
            break;
          case MO_LEX4:
            snprintf(s, N, "LexTiny => %d", p->nvars);
            break;
          case MO_GREVLEX:
            snprintf(s, N, "GRevLex => ");
            p_ones = 1;
            break;
          case MO_GREVLEX2:
            snprintf(s, N, "GRevLexSmall => ");
            p_ones = 1;
            break;
          case MO_GREVLEX4:
            snprintf(s, N, "GRevLexTiny => ");
            p_ones = 1;
            break;
          case MO_GREVLEX_WTS:
            snprintf(s, N, "GRevLex => ");
            break;
          case MO_GREVLEX2_WTS:
            snprintf(s, N, "GRevLexSmall => ");
            break;
          case MO_GREVLEX4_WTS:
            snprintf(s, N, "GRevLexTiny => ");
            break;
          case MO_REVLEX:
            snprintf(s, N, "RevLex => %d", p->nvars);
            break;
          case MO_WEIGHTS:
            snprintf(s, N, "Weights => ");
            break;
          case MO_LAURENT:
            snprintf(s, N, "GroupLex => %d", p->nvars);
            break;
          case MO_LAURENT_REVLEX:
            snprintf(s, N, "GroupRevLex => %d", p->nvars);
            break;
          case MO_NC_LEX:
            snprintf(s, N, "NCLex => %d", p->nvars);
            break;
          case MO_POSITION_UP:
            snprintf(s, N, "Position => Up");
            break;
          case MO_POSITION_DOWN:
            snprintf(s, N, "Position => Down");
            break;
          default:
            snprintf(s, N, "UNKNOWN");
            break;
        }
      result = M2_join(result, M2_tostring(s));
      if (p->wts != nullptr)
        result = M2_join(result, intarray_to_string(p->nvars, p->wts));
      else if (p_ones)
        result = M2_join(result, ones_to_string(p->nvars));
    }
  result = M2_join(result, M2_tostring("\n    }"));
  return result;
}

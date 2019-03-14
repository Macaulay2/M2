#include "FreeMonoid.hpp"

FreeMonoid::FreeMonoid(
          const std::vector<std::string>& variableNames,
          const PolynomialRing* degreeRing,
          const std::vector<int>& degrees)
  : mVariableNames(variableNames),
    mDegreeRing(degreeRing),
    mDegrees(degrees)
{
  auto nvars = numVars();
  auto ndegrees = degreeMonoid().n_vars();
  assert(nvars * ndegrees == mDegrees.size());
  
  for (const int* i = mDegrees.data(); i != mDegrees.data() + mDegrees.size(); i += ndegrees)
    {
      int* deg = degreeMonoid().make_one();
      degreeMonoid().from_expvector(i, deg);
      mDegreeOfVar.push_back(deg);
    }
}

void FreeMonoid::one(MonomialInserter& m) const
{
  m.push_back(2);
  m.push_back(0);
}

void FreeMonoid::var(int v, MonomialInserter& m) const
{
  m.push_back(3);
  m.push_back(1);
  m.push_back(v);
}

bool FreeMonoid::is_one(const Monom& m) const
{
  return m[0] == 2;
}

int FreeMonoid::index_of_variable(const Monom& m) const
{
  if (m[0] != 3 or m[1] != 1) return -1;
  return m[2];
}

void FreeMonoid::copy(const Monom& m, MonomialInserter& result) const
{
  for (auto v : m) result.push_back(v);
  
  //  for (auto i = m.begin(); i != m.end(); ++i)
  //    result.push_back(*i);
  //  std::copy(m.begin(), m.end(), result);
}

void FreeMonoid::mult(const Monom& m1, const Monom& m2, MonomialInserter& result) const
{
  result.push_back(m1[0] + m2[0] - 2);
  result.push_back(m1[1] + m2[1]);
  // FM : Should we be using vector::insert?
  for (auto i = m1.begin()+2; i != m1.end(); ++i)
    result.push_back(*i);
  for (auto i = m2.begin()+2; i != m2.end(); ++i)
    result.push_back(*i);

  //  std::copy(std::begin(m1) + 2, std::end(m1), result);
  //  std::copy(std::begin(m2) + 2, std::end(m1), result);
}

void FreeMonoid::mult3(const Monom& m1, const Monom& m2, const Monom& m3, MonomialInserter& result) const
{
  result.push_back(m1[0] + m2[0] + m3[0] - 2);
  result.push_back(m1[1] + m2[1] + m3[1]);
  for (auto i = m1.begin()+2; i != m1.end(); ++i)
    result.push_back(*i);
  for (auto i = m2.begin()+2; i != m2.end(); ++i)
    result.push_back(*i);
  for (auto i = m3.begin()+2; i != m3.end(); ++i)
    result.push_back(*i);

  //  std::copy(std::begin(m1) + 2, std::end(m1), result);
  //  std::copy(std::begin(m2) + 2, std::end(m1), result);
}

int FreeMonoid::compare(const Monom& m1, const Monom& m2) const
{
  if (m1[1] > m2[1]) return GT;
  if (m1[1] < m2[1]) return LT;
  // at this stage, they have the same degree, so use lex order
  for (int j = 2; j < m1[0]; j++)
    {
      if (m1[j] > m2[j]) return LT;
      if (m1[j] < m2[j]) return GT;
    }
  // if we are here, the monomials are the same.
  return EQ;
}

void FreeMonoid::multi_degree(const Monom& m, int* already_allocated_degree_vector) const
{
  int* result = already_allocated_degree_vector; // just to use a smaller name...
  degreeMonoid().one(result); // reset value

  auto mon_length = m[0] - 2;
  auto mon_ptr = m + 2;
  for (auto j = 0; j < mon_length; j++)
    {
      degreeMonoid().mult(result, mDegreeOfVar[mon_ptr[j]], result);
    }
}
    
void FreeMonoid::elem_text_out(buffer& o, const Monom& m1) const
{
  auto mon_length = m1[0] - 2;
  auto mon_ptr = m1 + 2;
  for (auto j = 0; j < mon_length; j++)
    {
      // for now, just output the string.
      int curvar = mon_ptr[j];
      int curvarPower = 0;
      o << mVariableNames[curvar];
      while ((j < mon_length) && (mon_ptr[j] == curvar))
        {
          j++;
          curvarPower++;
        }
      if (curvarPower > 1) o << "^" << curvarPower;
      // back j up one since we went too far looking ahead.
      j--;
    }
}

// This function should reverse the order of the varpower terms.
// as the front end reverses the order of terms in a monomial.
void FreeMonoid::getMonomial(Monom monom, std::vector<int>& result) const
// Input is of the form: [len degree v1 v2 ... vn]
//                        where len = n + 2
// The output is of the following form, and appended to result.
// [2n+1 v1 e1 v2 e2 ... vn en], where each ei > 0, (in 'varpower' format)
// and the order is that of monom.  that is: a*b is encoded as [5, 0 1, 1 1] (commas are only for clarity)
{
  auto start = result.size();
  result.push_back(0);
  auto mon_length = monom[0] - 2;
  auto mon_ptr = monom + 2;
  for (auto j = 0; j < mon_length; j++)
    {
      int curvar = mon_ptr[j];
      int curvarPower = 0;
      result.push_back(curvar);
      while ((j < mon_length) && (mon_ptr[j] == curvar))
        {
          j++;
          curvarPower++;
        }
      result.push_back(curvarPower);
      // back j up one since we went too far looking ahead.
      --j;
    }
  result[start] = static_cast<int>(result.size() - start);
}

void FreeMonoid::getMonomialReversed(Monom monom, std::vector<int>& result) const
// Input is of the form: [len degree v1 v2 ... vn]
//                        where len = n + 2
// The output is of the following form, and appended to result.
// [2n+1 v1 e1 v2 e2 ... vn en], where each ei > 0, (in 'varpower' format)
// and the order is the OPPOSITE of monom.  that is: a*b is encoded as [5, 1 1, 0 1] (commas are only for clarity)
{
  auto start = result.size();
  result.push_back(0);
  auto mon_length = monom[0] - 2;
  auto mon_ptr = monom + 2;
  for (auto j = mon_length-1; j >= 0; --j)
    {
      int curvar = mon_ptr[j];
      int curvarPower = 0;
      result.push_back(curvar);
      while ((j >= 0) && (mon_ptr[j] == curvar))
        {
          --j;
          curvarPower++;
        }
      result.push_back(curvarPower);
      // back j up one since we went too far looking ahead.
      j++;
    }
  result[start] = static_cast<int>(result.size() - start);
}

// This function should reverse the order of the varpower terms
void FreeMonoid::fromMonomial(const int* monom, MonomialInserter& result) const
  // Input is of the form: [2n+1 v1 e1 v2 e2 ... vn en] (in 'varpower' format)
  // The output is of the following form, and stored in result.
  // [len deg v1 v2 v3 ... vn]
  // where len = n+2 and deg = sum of the degrees of the vi 
{
  int inputMonomLength = *monom;
  int monDeg = 0;
  int startMon = static_cast<int>(result.size());  
  // make a space for the length
  result.push_back(0);
  // make a space for the degree
  result.push_back(0);
  for (int j = inputMonomLength-2; j >= 1; j -= 2)
    {
      auto v = monom[j];
      int degv = 1;
      for (int k = 0; k < monom[j+1]; k++)
        {
          monDeg += degv;
          result.push_back(v);
        }
    }
  result[startMon] = static_cast<int>(result.size() - startMon);
  result[startMon+1] = monDeg;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:

/******************************************************************************
This file is part of CYTools.

CYTools is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

CYTools is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with CYTools.  If not, see <https://www.gnu.org/licenses/>.
******************************************************************************/

#include "mpreal.h"

#include <math.h>
#include <ctype.h>
#include <unistd.h>

#include <mutex>
#include <vector>
#include <string>
#include <thread>
#include <cstdlib>
#include <iostream>
#include <algorithm>
#include <unordered_map>
#include <unordered_set>

//// MES: added this to the file /////////////
using CurveAndGVCollection =
    std::vector < std::pair<std::vector<int>, mpz_srcptr> >;

// struct curveAndGVCollection
// {
//   MatrixConstructor mMatrix;

//   curveAndGVCollection(int h11) mMatrix(globalZZ->make_FreeModule(h11 + 1));
  
//   void add(const std::vector<int>& curve, mpfr_t gv); // throws an error if not allowed.
// }  
//// Above this: added to file ///////////////

// This struct is needed to create sets of vectors
struct VectorHash {
  size_t operator()(const std::vector<int>& v) const {
    size_t seed = v.size();
    size_t x;
    for(auto c : v) {
      x = c;
      x = ((x >> 16) ^ x) + 0xdf3b45d9f3b;
      x = ((x << 16) ^ x) + 0xdf3b45d9f3b;
      seed ^= x + 0x779b99e3779b9 + (seed << 12) + (seed >> 4);
    }
    return seed;
  }
};

// Define new types for convenience.
typedef mpfr::mpreal MPFloat;
typedef std::vector<int> IntVector;
typedef std::vector<std::vector<int> > IntMatrix;
typedef std::vector<std::vector<int> > VectorList;
typedef std::unordered_set<std::vector<int>,VectorHash> VectorSet;
typedef std::unordered_map<int,std::vector<std::pair<int,MPFloat> > > IntToH22GVDict;
typedef std::unordered_map<std::vector<int>,int,VectorHash> VecToIntDict;

// Define a polynomial struct
struct Polynomial {
  std::unordered_map<int,MPFloat> coeffs;
  std::vector<int> nonzero;
};

typedef std::unordered_map<int,Polynomial> IntToPolyDict;
typedef std::unordered_map<std::vector<int>,Polynomial,VectorHash> VecToPolyDict;

// Returns available RAM
unsigned long long GetAvailableSystemMemory()
{
#ifdef __linux__
  long pages = sysconf(_SC_AVPHYS_PAGES);
  long page_size = sysconf(_SC_PAGE_SIZE);
  return pages * page_size;
#elif __APPLE__
  // While I think of a better solution
  return 1000000;
#else
  return 1000000;
#endif
}

// Compute factorials with multiple precision
MPFloat mpfactorial(int z) {
  MPFloat res = z;
  if (z < 0) {
    throw std::invalid_argument("Input must be non-negative.");
  }
  else if (z == 0) {
    res = 1;
  }
  for (int i = z-1; i > 1; i--){
    res *= i;
  }
  return res;
}

// Compute digamma for integers with multiple precision
MPFloat mpdigamma(int z, const MPFloat &neg_em) {
  if (z <= 0) {
    throw std::invalid_argument("Digamma is infinite or complex.");
  }
  MPFloat res = neg_em;
  MPFloat tmpVar;
  for (int i = 1; i < z; i++){
    tmpVar = i;
    res += 1/tmpVar;
  }
  return res;
}

// Compute trigamma for integers with multiple precision
MPFloat mptrigamma(int z, const MPFloat &pi2d6) {
  if (z <= 0) {
    throw std::invalid_argument("Trigamma is infinite or complex.");
  }
  MPFloat res = pi2d6;
  MPFloat tmpVar;
  for (int i = 1; i < z; i++){
    tmpVar = i;
    res -= 1/(tmpVar*tmpVar);
  }
  return res;
}

// Parse a vector from std::cin
std::vector<int> ReadVector(std::istream &ist) {
  char c;
  int i;
  std::vector<int> vec;
  ist >> std::ws >> c;
  if (c == '(' || c == '[' || c == '{') {
    while (ist >> std::ws >> c) {
      if (c == ')' || c == ']' || c == '}') {
        break;
      }
      else if (isspace(c)) {
        continue;
      }
      else if (c == ',') {
        continue;
      }
      else if(isdigit(c) || c == '-') {
        ist.putback(c);
        ist >> i;
        vec.push_back(i);
      }
      else {
        throw std::invalid_argument("Failed to parse input as integer.");
      }
    }
  }
  else {
    throw std::invalid_argument("Missing vector opening bracket.");
  }
  ist.clear(std::ios::goodbit);
  return vec;
}

// Parse a vector of vectors from std::cin
std::vector<std::vector<int> > ReadVectors(std::istream &ist,
                                           bool allow_diff_lengths=false) {
  char c;
  unsigned int vec_size = 0;
  std::vector<std::vector<int> > vecs;
  std::vector<int> tmp_vec;
  ist >> std::ws >> c;
  if (c == '(' || c == '[' || c == '{') {
    while (ist >> std::ws >> c) {
      if (c == ')' || c == ']' || c == '}') {
        break;
      }
      else if (isspace(c)) {
        continue;
      }
      else if (c == ',') {
        continue;
      }
      else if (c == '(' || c == '[' || c == '{') {
        ist.putback(c);
        tmp_vec = ReadVector(ist);
        if (!tmp_vec.size()){
          throw std::invalid_argument("Read vector with zero size.");
        }
        if (!vec_size) {
          vec_size = tmp_vec.size();
        }
        else if (!allow_diff_lengths && vec_size != tmp_vec.size()){
          throw std::invalid_argument("All vectors must be the same size.");
        }
        vecs.push_back(tmp_vec);
      }
      else {
        throw std::invalid_argument("Missing vector opening bracket.");
      }
    }
  }
  else{
    throw std::invalid_argument("Missing matrix opening bracket.");
  }
  ist.clear(std::ios::goodbit);
  return vecs;
}

// Completes a given list of curves by making sure that all possible sums are
// in the list up to the specified maximum degree.
void CheckCompletenessThr(VectorSet &vset, const IntMatrix &check_vecs,
                          IntMatrix &new_vecs,
                          const IntMatrix &hilbert_basis,
                          IntVector &grading_vec, int max_deg,
                          int &curr_task, int n_curves, std::mutex &m,
                          std::mutex &task_mut, int min_mem) {
  int h11 = check_vecs[0].size();
  int tmp_deg;
  std::vector<int> tmp_vec(h11);
  while (true) {
    if (GetAvailableSystemMemory() < min_mem) {
      throw std::runtime_error("Memory is running low. "
                               "Exitting to prevent crash...");
    }
    task_mut.lock();
    int i = curr_task;
    curr_task++;
    task_mut.unlock();
    if (i >= n_curves) {
      break;
    }
    for (unsigned int j = 0; j < hilbert_basis.size(); j++) {
      tmp_deg = 0;
      for (int k = 0; k < h11; k++) {
        tmp_vec[k] = check_vecs[i][k] + hilbert_basis[j][k];
        tmp_deg += tmp_vec[k] * grading_vec[k];
      }
      // Discard if the vector exceeds the maximum degree
      if (tmp_deg > max_deg) {
        continue;
      }
      // Search in the set and add if missing
      m.lock();
      auto search = vset.find(tmp_vec);
      if (search != vset.end()){
        m.unlock();
        continue;
      }
      vset.insert(tmp_vec);
      new_vecs.push_back(tmp_vec);
      m.unlock();
    }
  }
}

// Removes curves that are not in the Hilbert basis. This is done by checking
// that curves are not sums of up to 5 other curves.
void RemoveNonGensThr(const VectorSet &vset, const IntMatrix &vecs,
                      IntMatrix &vecs_to_remove, const IntVector &grading_vec,
                      int max_deg, int &curr_task, int n_curves, std::mutex &m,
                      std::mutex &task_mut) {
  int h11 = vecs[0].size();
  int tmp_deg;
  std::vector<int> tmp_vec(h11);
  while (true) {
    task_mut.lock();
    int i = curr_task;
    curr_task++;
    task_mut.unlock();
    if (i >= n_curves) {
      break;
    }
    for (int j = i; j < n_curves; j++) {
      tmp_deg = 0;
      for (int k = 0; k < h11; k++) {
        tmp_vec[k] = vecs[i][k] + vecs[j][k];
        tmp_deg += tmp_vec[k] * grading_vec[k];
      }
      // Discard if the vector exceeds the maximum degree
      if (tmp_deg > max_deg) {
        continue;
      }
      // Search in the set and erase if found
      auto search2 = vset.find(tmp_vec);
      if (search2 != vset.end()) {
        m.lock();
        vecs_to_remove.push_back(tmp_vec);
        m.unlock();
      }
      for (unsigned int k = j; k < vecs.size(); k++) {
        tmp_deg = 0;
        for (int kk = 0; kk < h11; kk++) {
          tmp_vec[kk] = vecs[i][kk] + vecs[j][kk] + vecs[k][kk];
          tmp_deg += tmp_vec[kk] * grading_vec[kk];
        }
        // Discard if the vector exceeds the maximum degree
        if (tmp_deg > max_deg) {
          continue;
        }
        // Search in the set and erase if found
        auto search3 = vset.find(tmp_vec);
        if (search3 != vset.end()) {
          m.lock();
          vecs_to_remove.push_back(tmp_vec);
          m.unlock();
        }
        for (unsigned int l = k; l < vecs.size(); l++) {
          tmp_deg = 0;
          for (int kk = 0; kk < h11; kk++) {
            tmp_vec[kk] = (vecs[i][kk] + vecs[j][kk] + vecs[k][kk]
                           + vecs[l][kk]);
            tmp_deg += tmp_vec[kk] * grading_vec[kk];
          }
          // Discard if the vector exceeds the maximum degree
          if (tmp_deg > max_deg) {
            continue;
          }
          // Search in the set and erase if found
          auto search4 = vset.find(tmp_vec);
          if (search4 != vset.end()) {
            m.lock();
            vecs_to_remove.push_back(tmp_vec);
            m.unlock();
          }
          for (unsigned int ll = l; ll < vecs.size(); ll++) {
            tmp_deg = 0;
            for (int kk = 0; kk < h11; kk++) {
              tmp_vec[kk] = (vecs[i][kk] + vecs[j][kk] + vecs[k][kk]
                              + vecs[l][kk] + vecs[ll][kk]);
              tmp_deg += tmp_vec[kk] * grading_vec[kk];
            }
            // Discard if the vector exceeds the maximum degree
            if (tmp_deg > max_deg) {
              continue;
            }
            // Search in the set and erase if found
            auto search5 = vset.find(tmp_vec);
            if (search5 != vset.end()) {
              m.lock();
              vecs_to_remove.push_back(tmp_vec);
              m.unlock();
            }
          }
        }
      }
    }
  }
}

// Fills out the relevant part of the past light cone
void FillPastLightConeThr(VectorSet &vset, const IntMatrix &check_vecs,
                          const VectorSet &pset, IntMatrix &new_vecs,
                          const IntMatrix &hilbert_basis,
                          IntVector &grading_vec, int max_deg, int &curr_task,
                          int n_curves, std::mutex &m, std::mutex &task_mut,
                          int min_mem) {
  int h11 = check_vecs[0].size();
  int tmp_deg;
  std::vector<int> tmp_vec(h11);
  while (true) {
    if (GetAvailableSystemMemory() < min_mem) {
      throw std::runtime_error("Memory is running low. "
                               "Exitting to prevent crash...");
    }
    task_mut.lock();
    int i = curr_task;
    curr_task++;
    task_mut.unlock();
    if (i >= n_curves) {
      break;
    }
    for (unsigned int j = 0; j < hilbert_basis.size(); j++) {
      tmp_deg = 0;
      for (int k = 0; k < h11; k++) {
        tmp_vec[k] = check_vecs[i][k] + hilbert_basis[j][k];
        tmp_deg += tmp_vec[k] * grading_vec[k];
      }
      // Discard if the vector exceeds the maximum degree
      if (tmp_deg > 2*max_deg) {
        continue;
      }
      // Discard if the vector is not in the past light cone
      auto search = pset.find(tmp_vec);
      if (search == pset.end()) {
        continue;
      }
      // Search in the set and add if missing
      m.lock();
      search = vset.find(tmp_vec);
      if (search != vset.end()){
        m.unlock();
        continue;
      }
      vset.insert(tmp_vec);
      new_vecs.push_back(tmp_vec);
      m.unlock();
    }
  }
}

// Computes half of the past light cone. I.e. it takes the cone that was
// computed and flips it and sets the apex to be the relevant curve.
void HalfPastLightConeThr(VectorSet &vset, VectorSet &f_vset,
                          std::vector<int> &vec, std::mutex &m) {
  int h11 = vec.size();
  std::vector<int> tmp_vec(h11);
  for (auto &vec2 : vset) {
    for (int i = 0; i < h11; i++) {
      tmp_vec[i] = vec[i] - vec2[i];
    }
    auto search = f_vset.find(tmp_vec);
    if (search != vset.end()){
      continue;
    }
    m.lock();
    f_vset.insert(tmp_vec);
    m.unlock();
  }
}

// Trims the remaining curves that are not in the past light cone
void TrimPastLightConeThr(VectorSet &vset, VectorSet &f_vset,
                          std::vector<int> &vec, std::mutex &m) {
  int h11 = vec.size();
  std::vector<int> tmp_vec(h11);
  for (auto &vec2 : vset) {
    for (int i = 0; i < h11; i++) {
      tmp_vec[i] = vec[i] - vec2[i];
    }
    // Search in the original and final set and add if missing
    auto search = vset.find(tmp_vec);
    if (search == vset.end()){
      continue;
    }
    search = f_vset.find(tmp_vec);
    if (search != vset.end()){
      continue;
    }
    m.lock();
    f_vset.insert(tmp_vec);
    m.unlock();
  }
}

// Prints polynomials
void PolyPrint(const Polynomial &p, char coord_name = 'z') {
  for (unsigned int i = 0; i < p.nonzero.size(); i++) {
    std::cerr << (i ? " + " : "");
    std::cerr << p.coeffs.at(p.nonzero[i]) << "*" << coord_name << "^{n_" << p.nonzero[i] << "}";
  }
  std::cerr << std::endl;
}
void PolyPrint(const Polynomial &p, const IntMatrix &curves,
               char coord_name = 'z') {
  int h11 = curves[0].size();
  for (unsigned int i = 0; i < p.nonzero.size(); i++) {
    std::cerr << (i ? " + " : "");
    std::cerr << p.coeffs.at(i);
    for (int j = 0; j < h11; j++) {
      if (!curves[i][j]) {
        continue;
      }
      std::cerr << "*" << coord_name << "_" << i;
      if (curves[i][j] != 1) {
        std::cerr << "^" << curves[i][j];
      }
    }
  }
  std::cerr << std::endl;
}

// Computes the product of two polynomials
Polynomial PolyProd(const Polynomial &p1, const Polynomial &p2,
                    const IntMatrix &curves, const IntVector &degs,
                    const VecToIntDict &curve_dict) {
  Polynomial res;
  int d1, d2, ind;
  int h11 = curves[0].size();
  int max_deg = degs[degs.size()-1];
  IntVector tmp_vec_h11(h11);
  VecToIntDict::const_iterator search;
  std::unordered_map<int,MPFloat>::const_iterator search2;
  const Polynomial *pshort;
  const Polynomial *plong;
  if (p1.nonzero.size() < p2.nonzero.size()) {
    pshort = &p1;
    plong = &p2;
  }
  else {
    pshort = &p2;
    plong = &p1;
  }
  for (auto &i : pshort->nonzero) {
    d1 = degs[i];
    for (auto &j : plong->nonzero) {
      d2 = degs[j];
      if (d1+d2 > max_deg) {
        break;
      }
      for (int k = 0; k < h11; k++) {
        tmp_vec_h11[k] = curves[i][k] + curves[j][k];
      }
      search = curve_dict.find(tmp_vec_h11);
      if (search == curve_dict.end()) {
        continue;
      }
      ind = search->second;
      search2 = res.coeffs.find(ind);
      if (search2 == res.coeffs.end()) {
        res.coeffs[ind] = pshort->coeffs.at(i) * plong->coeffs.at(j);
        res.nonzero.push_back(ind);
      }
      else {
        res.coeffs[ind] += pshort->coeffs.at(i) * plong->coeffs.at(j);
      }
    }
  }
  std::sort(res.nonzero.begin(), res.nonzero.end());
  return res;
}

// Finds the degree of the monomial with the smallest degree (with non-zero
// coefficient)
int PolyMinDeg(const Polynomial &p, const IntVector &degs) {
  for (auto &i : p.nonzero) {
    return degs[i];
  }
  return degs[degs.size()-1] + 1;
}

// Truncates a polytop to a certain degree
Polynomial PolyTrunc(const Polynomial &p, int n, const IntVector &degs) {
  Polynomial res;
  for (auto &i : p.nonzero) {
    if (degs[i] > n) {
      break;
    }
    res.nonzero.push_back(i);
    res.coeffs[i] = p.coeffs.at(i);
  }
  return res;
}

// Computes the product of a polynomial with a scalar in place
void PolyProdScalarIP(Polynomial &p, int f) {
  for (auto &c : p.coeffs) {
    c.second *= f;
  }
}
void PolyProdScalarIP(Polynomial &p, const MPFloat &f) {
  for (auto &c : p.coeffs) {
    c.second *= f;
  }
}

// Computes the division of a polynomial by a scalar in place
void PolyDivIP(Polynomial &p, int d) {
  for (auto &c : p.coeffs) {
      c.second /= d;
  }
}
void PolyDivIP(Polynomial &p, const MPFloat &d) {
  for (auto &c : p.coeffs) {
      c.second /= d;
  }
}

// Computes the sum of polynomials in place
void PolySumIP(Polynomial &p1, const Polynomial &p2) {
  bool resort = false;
  std::unordered_map<int,MPFloat>::const_iterator search;
  for (auto &c : p2.coeffs) {
    search = p1.coeffs.find(c.first);
    if (search == p1.coeffs.end()) {
      p1.coeffs[c.first] = c.second;
      p1.nonzero.push_back(c.first);
      resort = true;
    }
    else {
      p1.coeffs[c.first] += c.second;
    }
  }
  if (resort) {
    std::sort(p1.nonzero.begin(), p1.nonzero.end());
  }
}

// Computes the difference of polynomials in place
void PolySubIP(Polynomial &p1, const Polynomial &p2) {
  bool resort = false;
  std::unordered_map<int,MPFloat>::const_iterator search;
  for (auto &c : p2.coeffs) {
    search = p1.coeffs.find(c.first);
    if (search == p1.coeffs.end()) {
      p1.coeffs[c.first] = -c.second;
      p1.nonzero.push_back(c.first);
      resort = true;
    }
    else {
      p1.coeffs[c.first] -= c.second;
    }
  }
  if (resort) {
    std::sort(p1.nonzero.begin(), p1.nonzero.end());
  }
}

// Computes the sum of polynomials in place, with an additional factor
void PolySumFactIP(Polynomial &p1, const Polynomial &p2, int f) {
  bool resort = false;
  std::unordered_map<int,MPFloat>::const_iterator search;
  for (auto &c : p2.coeffs) {
    search = p1.coeffs.find(c.first);
    if (search == p1.coeffs.end()) {
      p1.coeffs[c.first] = c.second * f;
      p1.nonzero.push_back(c.first);
      resort = true;
    }
    else {
      p1.coeffs[c.first] += c.second * f;
    }
  }
  if (resort) {
    std::sort(p1.nonzero.begin(), p1.nonzero.end());
  }
}

// Computes the sum of polynomials in place, with an additional division
void PolySumDivIP(Polynomial &p1, const Polynomial &p2, int d) {
  bool resort = false;
  std::unordered_map<int,MPFloat>::const_iterator search;
  for (auto &c : p2.coeffs) {
    search = p1.coeffs.find(c.first);
    if (search == p1.coeffs.end()) {
      p1.coeffs[c.first] = c.second / d;
      p1.nonzero.push_back(c.first);
      resort = true;
    }
    else {
      p1.coeffs[c.first] += c.second / d;
    }
  }
  if (resort) {
    std::sort(p1.nonzero.begin(), p1.nonzero.end());
  }
}
void PolySumDivIP(Polynomial &p1, const Polynomial &p2, const MPFloat &d) {
  bool resort = false;
  std::unordered_map<int,MPFloat>::const_iterator search;
  for (auto &c : p2.coeffs) {
    search = p1.coeffs.find(c.first);
    if (search == p1.coeffs.end()) {
      p1.coeffs[c.first] = c.second / d;
      p1.nonzero.push_back(c.first);
      resort = true;
    }
    else {
      p1.coeffs[c.first] += c.second / d;
    }
  }
  if (resort) {
    std::sort(p1.nonzero.begin(), p1.nonzero.end());
  }
}

// Computes the inverse of a polynomial
Polynomial PolyInv(const Polynomial &p, const IntMatrix &curves,
                   const IntVector &degs, const VecToIntDict &curve_dict) {
  Polynomial res;
  res.nonzero.push_back(0);
  res.coeffs[0] = 1;
  if (p.nonzero[0] != 0 || !p.coeffs.at(0)) {
    throw std::invalid_argument("Polynomial is not invertible.");
  }
  int max_deg = degs[degs.size()-1];
  Polynomial p0 = p;
  p0.nonzero.erase(p0.nonzero.begin());
  p0.coeffs.erase(0);
  PolyDivIP(p0, p.coeffs.at(0));
  Polynomial tmp_poly = res;
  int min_deg = PolyMinDeg(p0, degs);
  for (int i = 1; i < 1+max_deg/min_deg; i++) {
    tmp_poly = PolyProd(tmp_poly, p0, curves, degs, curve_dict);
    if (i&1) {
      PolySumFactIP(res, tmp_poly, -1);
    }
    else {
      PolySumIP(res, tmp_poly);
    }
  }
  PolyDivIP(res, p.coeffs.at(0));
  return res;
}

// Computes the nonnegative power of a polynomial
Polynomial PolyPow(const Polynomial &p, unsigned int n,
                   const IntMatrix &curves, const IntVector &degs,
                   const VecToIntDict &curve_dict) {
  Polynomial res;
  res.nonzero.push_back(0);
  res.coeffs[0] = 1;
  if (n == 0) {
    return res;
  }
  else if (n == 1) {
    res = p;
    return res;
  }
  int max_deg = degs[degs.size()-1];
  Polynomial tmp_poly = PolyTrunc(p, max_deg-(n-1)*PolyMinDeg(p, degs), degs);
  while (true) {
    if (n & 1) {
      res = PolyProd(res, tmp_poly, curves, degs, curve_dict);
    }
    n >>= 1;
    if (!n) {
      break;
    }
    tmp_poly = PolyProd(tmp_poly, tmp_poly, curves, degs, curve_dict);
  }
  return res;
}

// Computes the exponential of a polynomial (and its negative)
void PolyExp(Polynomial &res_pos, Polynomial &res_neg, const Polynomial &p,
             const IntMatrix &curves, const IntVector &degs,
             const VecToIntDict &curve_dict) {
  bool has_const = (p.nonzero.size() > 0 && p.nonzero[0] == 0);
  res_pos = Polynomial();
  res_pos.nonzero.push_back(0);
  res_pos.coeffs[0] = 1;
  res_neg = res_pos;
  Polynomial p0 = p;
  if (has_const) {
    p0.nonzero.erase(p0.nonzero.begin());
    p0.coeffs.erase(0);
  }
  Polynomial tmp_poly = res_pos;
  int min_deg = PolyMinDeg(p0, degs);
  int max_deg = degs[degs.size()-1];
  MPFloat tmp_var;
  for (int i = 1; i < 1+max_deg/min_deg; i++) {
    tmp_poly = PolyProd(tmp_poly, p0, curves, degs, curve_dict);
    tmp_var = mpfactorial(i);
    PolySumDivIP(res_pos, tmp_poly, tmp_var);
    if (i&1) {
      tmp_var *= -1;
    }
    PolySumDivIP(res_neg, tmp_poly, tmp_var);
  }
  if (has_const) {
    tmp_var = exp(p.coeffs.at(0));
    PolyProdScalarIP(res_pos, tmp_var);
    PolyProdScalarIP(res_neg, tmp_var);
  }
}

// Computes the dilogarithm of a polynomial
Polynomial PolyLi2(const Polynomial &p, const IntMatrix &curves,
                   const IntVector &degs, const VecToIntDict &curve_dict) {
  if ((p.nonzero.size() > 0 && p.nonzero[0] == 0)
        || p.coeffs.find(0) != p.coeffs.end()) {
      std::cout << "hi" << std::endl;
      PolyPrint(p);
      throw std::invalid_argument("This type of polynomials are not "
                                  "supported.");
  }
  Polynomial res = p;
  Polynomial tmp_poly = p;
  int min_deg = PolyMinDeg(p, degs);
  int max_deg = degs[degs.size()-1];
  for (int i = 2; i < 1+max_deg/min_deg; i++) {
    tmp_poly = PolyProd(tmp_poly, p, curves, degs, curve_dict);
    PolySumDivIP(res, tmp_poly, i*i);
  }
  return res;
}

// Removes coefficients with a very small magnitude
void PolyCleanUp(Polynomial &p, MPFloat &cutoff, bool force_resort=false) {
  std::vector<int> new_nonzero;
  for (auto &i : p.nonzero) {
    if (abs(p.coeffs[i]) < cutoff) {
      p.coeffs.erase(i);
    }
    else {
      new_nonzero.push_back(i);
    }
  }
  if (force_resort) {
    std::sort(new_nonzero.begin(), new_nonzero.end());
  }
  p.nonzero = new_nonzero;
}

// Computes c for curves with zero negative entries in the dot product with Q
void computeC_0neg(Polynomial &c0, std::vector<Polynomial> &c1,
                   VecToPolyDict &c2, const std::vector<int> &neg0,
                   const IntMatrix &Q, const IntMatrix &Q0s,
                   const IntMatrix &curves_dot_Q,
                   const IntMatrix &curves_dot_Q0s, int prec,
                   std::mutex &c0mutex, std::mutex &c1mutex,
                   std::mutex &c2mutex, int h11, int h11pd, int min_mem,
                   int &curr_task, std::mutex &task_mut, VectorSet &beta_pairs,
                   const MPFloat &neg_em, const MPFloat &pi2d6) {
  mpfr::mpreal::set_default_prec(prec);
  std::vector<MPFloat> A(h11);
  MPFloat tmp_num, c0fact, tmp_final;
  int i, i_ind, a, b;
  while (true) {
    task_mut.lock();
    i_ind = curr_task;
    curr_task++;
    task_mut.unlock();
    if (i_ind >= neg0.size()) {
      break;
    }
    i = neg0[i_ind];
    if (GetAvailableSystemMemory() < min_mem) {
      throw std::runtime_error("Memory is running low. "
                               "Exitting to prevent crash...");
    }
    c0fact = mpfactorial(curves_dot_Q0s[0][i]);
    for (int j = 1; j < Q0s.size(); j++){
      c0fact *= mpfactorial(curves_dot_Q0s[j][i]);
    }
    for (int j = 0; j < h11pd; j++) {
      c0fact /= mpfactorial(curves_dot_Q[i][j]);
    }
    c0mutex.lock();
    c0.coeffs[i] = c0fact;
    c0.nonzero.push_back(i);
    c0mutex.unlock();
    for (int a = 0; a < h11; a++) {
      A[a] = Q0s[0][a]*mpdigamma(curves_dot_Q0s[0][i]+1, neg_em);
      for (int j = 1; j < Q0s.size(); j++){
        A[a] += Q0s[j][a]*mpdigamma(curves_dot_Q0s[j][i]+1, neg_em);
      }
      for (int j = 0; j < h11pd; j++) {
        A[a] -= Q[a][j]*mpdigamma(curves_dot_Q[i][j]+1, neg_em);
      }
      tmp_final = c0fact*A[a];
      c1mutex.lock();
      c1[a].coeffs[i] = tmp_final;
      c1[a].nonzero.push_back(i);
      c1mutex.unlock();
    }
    for (auto &pp : beta_pairs) {
      a = pp[0];
      b = pp[1];
      tmp_num = Q0s[0][a]*Q0s[0][b]*mptrigamma(curves_dot_Q0s[0][i]+1, pi2d6);
      for (int j = 1; j < Q0s.size(); j++){
        tmp_num += (Q0s[j][a]*Q0s[j][b]
                    *mptrigamma(curves_dot_Q0s[j][i]+1, pi2d6));
      }
      for (int j = 0; j < h11pd; j++) {
        tmp_num -= Q[a][j]*Q[b][j]*mptrigamma(curves_dot_Q[i][j]+1, pi2d6);
      }
      tmp_num += A[a]*A[b];
      tmp_final = c0fact*tmp_num;
      c2mutex.lock();
      c2[{a,b}].coeffs[i] = tmp_final;
      c2[{a,b}].nonzero.push_back(i);
      c2mutex.unlock();
    }
  }
  mpfr_free_cache();
}

// Computes c for curves with one negative entry in the dot product with Q
void computeC_1neg(std::vector<Polynomial> &c1, VecToPolyDict &c2,
                   const std::vector<std::tuple<int,int> > &neg1,
                   const IntMatrix &Q, const IntMatrix &Q0s,
                   const IntMatrix &curves_dot_Q,
                   const IntMatrix &curves_dot_Q0s, int prec,
                   std::mutex &c1mutex, std::mutex &c2mutex, int h11,
                   int h11pd, int min_mem, int &curr_task,
                   std::mutex &task_mut, VectorSet &beta_pairs,
                   const MPFloat &neg_em) {
  mpfr::mpreal::set_default_prec(prec);
  std::vector<MPFloat> A(h11);
  MPFloat tmp_fact, tmp_final;
  int i, k, m, sn;
  int i_ind, a, b;
  while (true) {
    task_mut.lock();
    i_ind = curr_task;
    curr_task++;
    task_mut.unlock();
    if (i_ind >= neg1.size()) {
      break;
    }
    if (GetAvailableSystemMemory() < min_mem) {
      throw std::runtime_error("Memory is running low. "
                               "Exitting to prevent crash...");
    }
    i = std::get<0>(neg1[i_ind]);
    k = std::get<1>(neg1[i_ind]);
    m = curves_dot_Q[i][k];
    sn = (m&1 ? 1 : -1);
    tmp_fact = mpfactorial(curves_dot_Q0s[0][i]);
    for (int j = 1; j < Q0s.size(); j++){
      tmp_fact *= mpfactorial(curves_dot_Q0s[j][i]);
    }
    tmp_fact *= mpfactorial(-m-1);
    for (int j = 0; j < h11pd; j++) {
      if (j == k) {
        continue;
      }
      tmp_fact /= mpfactorial(curves_dot_Q[i][j]);
    }
    for (int a = 0; a < h11; a++) {
      A[a] = Q0s[0][a]*mpdigamma(curves_dot_Q0s[0][i]+1, neg_em);
      for (int j = 1; j < Q0s.size(); j++){
        A[a] += Q0s[j][a]*mpdigamma(curves_dot_Q0s[j][i]+1, neg_em);
      }
      for (int j = 0; j < h11pd; j++) {
        A[a] -= Q[a][j]*mpdigamma((j==k ? -m : curves_dot_Q[i][j]+1), neg_em);
      }
      tmp_final = (sn*Q[a][k])*tmp_fact;
      c1mutex.lock();
      c1[a].coeffs[i] = tmp_final;
      c1[a].nonzero.push_back(i);
      c1mutex.unlock();
    }
    for (auto &pp : beta_pairs) {
      a = pp[0];
      b = pp[1];
      tmp_final = (sn*(Q[a][k]*A[b]+Q[b][k]*A[a]))*tmp_fact;
      c2mutex.lock();
      c2[{a,b}].coeffs[i] = tmp_final;
      c2[{a,b}].nonzero.push_back(i);
      c2mutex.unlock();
      }
  }
  mpfr_free_cache();
}

// Computes c for curves with two negative entries in the dot product with Q
void computeC_2neg(VecToPolyDict &c2,
                   const std::vector<std::tuple<int,int,int> > &neg2,
                   const IntMatrix &Q, const IntMatrix &Q0s,
                   const IntMatrix &curves_dot_Q,
                   const IntMatrix &curves_dot_Q0s, int prec,
                   std::mutex &c2mutex, int h11, int h11pd, int min_mem,
                   int &curr_task, std::mutex &task_mut,
                   VectorSet &beta_pairs) {
  mpfr::mpreal::set_default_prec(prec);
  std::vector<MPFloat> A(h11);
  MPFloat c0fact, tmp_num;
  int i, k0, k1, m0, m1;
  int i_ind, a, b;
  while (true) {
    task_mut.lock();
    i_ind = curr_task;
    curr_task++;
    task_mut.unlock();
    if (i_ind >= neg2.size()) {
      break;
    }
    if (GetAvailableSystemMemory() < min_mem) {
      throw std::runtime_error("Memory is running low. "
                               "Exitting to prevent crash...");
    }
    i = std::get<0>(neg2[i_ind]);
    k0 = std::get<1>(neg2[i_ind]);
    k1 = std::get<2>(neg2[i_ind]);
    m0 = curves_dot_Q[i][k0];
    m1 = curves_dot_Q[i][k1];
    c0fact = ((m0+m1)&1 ? -1 : 1);
    c0fact *= mpfactorial(curves_dot_Q0s[0][i]);
    for (int j = 1; j < Q0s.size(); j++){
      c0fact *= mpfactorial(curves_dot_Q0s[j][i]);
    }
    c0fact *= mpfactorial(-m0-1);
    c0fact *= mpfactorial(-m1-1);
    for (int j = 0; j < h11pd; j++) {
      if (j == k0 || j== k1) {
        continue;
      }
      c0fact /= mpfactorial(curves_dot_Q[i][j]);
    }
    for (auto &pp : beta_pairs) {
      a = pp[0];
      b = pp[1];
      tmp_num = c0fact*(Q[a][k0]*Q[b][k1]+Q[a][k1]*Q[b][k0]);
      c2mutex.lock();
      c2[{a,b}].coeffs[i] = tmp_num;
      c2[{a,b}].nonzero.push_back(i);
      c2mutex.unlock();
    }
  }
  mpfr_free_cache();
}

// Computes the inverse of c0
void ComputeInvC0Thr(Polynomial &c0inv, const Polynomial &c0,
                     const IntMatrix &curves, const IntVector &degs,
                     const VecToIntDict &curve_dict, int prec) {
  mpfr::mpreal::set_default_prec(prec);
  c0inv = PolyInv(c0, curves, degs, curve_dict);
  mpfr_free_cache();
}

// Computes the beta polynomials
void ComputeAlphaThr(std::vector<Polynomial> &alpha,
                     const Polynomial &c0inv,
                     const std::vector<Polynomial> &c1,
                     const IntMatrix &curves, const IntVector &degs,
                     const VecToIntDict &curve_dict, int prec, int &curr_task,
                     std::mutex &task_mut, std::mutex &m, int min_mem) {
  mpfr::mpreal::set_default_prec(prec);
  Polynomial tmp_poly;
  int i;
  while (true) {
    task_mut.lock();
    i = curr_task;
    curr_task++;
    task_mut.unlock();
    if (i >= c1.size()) {
      break;
    }
    if (GetAvailableSystemMemory() < min_mem) {
      throw std::runtime_error("Memory is running low. "
                               "Exitting to prevent crash...");
    }
    tmp_poly = PolyProd(c0inv, c1.at(i), curves, degs, curve_dict);
    m.lock();
    alpha[i] = std::move(tmp_poly);
    m.unlock();
  }
  mpfr_free_cache();
}

// Computes the alpha or beta polynomials
void ComputeBetaThr(VecToPolyDict &beta,
                     const Polynomial &c0inv,
                     const VecToPolyDict &c2,
                     const IntMatrix &curves, const IntVector &degs,
                     const VecToIntDict &curve_dict, int prec, int &curr_task,
                     const VectorList &beta_pairs_vec,
                     std::mutex &task_mut, std::mutex &m, int min_mem) {
  mpfr::mpreal::set_default_prec(prec);
  Polynomial tmp_poly;
  int p_ind;
  std::vector<int> pp;
  while (true) {
    task_mut.lock();
    p_ind = curr_task;
    curr_task++;
    task_mut.unlock();
    if (p_ind >= beta_pairs_vec.size()) {
      break;
    }
    pp = beta_pairs_vec[p_ind];
    if (GetAvailableSystemMemory() < min_mem) {
      throw std::runtime_error("Memory is running low. "
                               "Exitting to prevent crash...");
    }
    tmp_poly = PolyProd(c0inv, c2.at(pp), curves, degs, curve_dict);
    if (tmp_poly.nonzero.size() > 0 && tmp_poly.nonzero[0] == 0) {
      tmp_poly.nonzero.erase(tmp_poly.nonzero.begin());
      tmp_poly.coeffs.erase(0);
    }
    m.lock();
    beta[pp] = std::move(tmp_poly);
    m.unlock();
  }
  mpfr_free_cache();
}

// Computes F polynomials
void ComputeFThr(VecToPolyDict &F,
                 const std::vector<Polynomial> &alpha,
                 const VecToPolyDict &beta,
                 const IntMatrix &curves, const IntVector &degs,
                 const VecToIntDict &curve_dict, int prec, int &curr_task,
                 const VectorList &beta_pairs_vec,
                 std::mutex &task_mut, std::mutex &m, int min_mem) {
  mpfr::mpreal::set_default_prec(prec);
  int h11 = alpha.size();
  Polynomial tmp_poly;
  MPFloat tmp_num;
  int a, b, p_ind;
  while (true) {
    task_mut.lock();
    p_ind = curr_task;
    curr_task++;
    task_mut.unlock();
    if (p_ind >= beta_pairs_vec.size()) {
      break;
    }
    a = beta_pairs_vec[p_ind][0];
    b = beta_pairs_vec[p_ind][1];
    if (GetAvailableSystemMemory() < min_mem) {
      throw std::runtime_error("Memory is running low. "
                               "Exitting to prevent crash...");
    }
    tmp_poly = PolyProd(alpha.at(a), alpha.at(b), curves, degs, curve_dict);
    PolySubIP(tmp_poly, beta.at({a,b}));
    PolyProdScalarIP(tmp_poly, -1);

    m.lock();
    F[{a,b}] = tmp_poly;
    m.unlock();
  }
  mpfr_free_cache();
}

// Computes instanton corrections
void ComputeInstThr(std::vector<Polynomial> &inst,
                    const VecToPolyDict &F,
                    const VecToIntDict &intnums,
                    const IntMatrix &curves, const IntVector &degs,
                    const VecToIntDict &curve_dict, int prec, int &curr_task,
                    std::mutex &task_mut, std::mutex &m, int h22, int cy_dim,
                    int min_mem) {
  mpfr::mpreal::set_default_prec(prec);
  std::vector<int> tmp_vec(3);
  int intnum, i;
  Polynomial tmp_poly;
  MPFloat tmp_num;
  VecToIntDict::const_iterator search;
  int h11 = curves[0].size();
  while (true) {
    task_mut.lock();
    i = curr_task;
    curr_task++;
    task_mut.unlock();
    if (i >= h22) {
      break;
    }
    if (GetAvailableSystemMemory() < min_mem) {
      throw std::runtime_error("Memory is running low. "
                               "Exitting to prevent crash...");
    }
    for (int a = 0; a < h11; a++){
      for (int b = a; b < h11; b++){
        tmp_vec = {i,a,b};
        if (cy_dim > 3) {
          if (a>b) {
            tmp_vec = {i,b,a};
          }
        }
        else {
          std::sort(tmp_vec.begin(), tmp_vec.end());
        }
        search = intnums.find(tmp_vec);
        if (search == intnums.end()) {
          continue;
        }
        intnum = search->second;
        tmp_poly = F.at({a,b});
        if (a!=b) {
          PolyProdScalarIP(tmp_poly, intnum);
        }
        else {
          tmp_num = intnum;
          tmp_num /= 2;
          PolyProdScalarIP(tmp_poly, tmp_num);
        }
        m.lock();
        PolySumIP(inst[i], tmp_poly);
        m.unlock();
      }
    }
  }
  mpfr_free_cache();
}

// Computes the exponentials of alpha polynomials
void ComputeExpAlphaThr(std::vector<Polynomial> &expalpha_pos,
                        std::vector<Polynomial> &expalpha_neg,
                        const std::vector<Polynomial> &alpha,
                        const IntMatrix &curves, const IntVector &degs,
                        const VecToIntDict &curve_dict, int prec,
                        int &curr_task, std::mutex &task_mut, std::mutex &m,
                        int min_mem) {
  mpfr::mpreal::set_default_prec(prec);
  Polynomial tmp_poly_pos, tmp_poly_neg;
  MPFloat tmp_num;
  int h11 = alpha.size();
  int i;
  while (true) {
    task_mut.lock();
    i = curr_task;
    curr_task++;
    task_mut.unlock();
    if (i >= h11) {
      break;
    }
    if (GetAvailableSystemMemory() < min_mem) {
      throw std::runtime_error("Memory is running low. "
                               "Exitting to prevent crash...");
    }
    tmp_poly_pos.nonzero.clear();
    tmp_poly_pos.coeffs.clear();
    tmp_poly_neg.nonzero.clear();
    tmp_poly_neg.coeffs.clear();
    PolyExp(tmp_poly_pos, tmp_poly_neg, alpha.at(i), curves, degs, curve_dict);
    m.lock();
    expalpha_pos[i] = std::move(tmp_poly_pos);
    expalpha_neg[i] = std::move(tmp_poly_neg);
    m.unlock();
  }
  mpfr_free_cache();
}

// Computes qN for generator curves
Polynomial ComputeqN(const Polynomial &closest_curve,
                     const IntVector &closest_curve_diff,
                     const std::vector<Polynomial> &expalpha_pos,
                     const std::vector<Polynomial> &expalpha_neg,
                     const IntMatrix &curves, const IntVector &degs,
                     const VecToIntDict &curve_dict) {
  int h11 = curves[0].size();
  Polynomial res = closest_curve;
  Polynomial tmp_poly;
  for (int i = 0; i < h11; i++) {
    if (!closest_curve_diff[i]) {
      continue;
    }
    else if (closest_curve_diff[i] > 0) {
      tmp_poly = PolyPow(expalpha_pos.at(i), closest_curve_diff[i], curves, degs, curve_dict);
    }
    else {
      tmp_poly = PolyPow(expalpha_neg.at(i), -closest_curve_diff[i], curves, degs, curve_dict);
    }
    res = PolyProd(res, tmp_poly, curves, degs, curve_dict);
  }
  return res;
}

// Computes qN and Li2(qN)
void ComputeLi2qNThr(const std::vector<int> &qN_to_compute,
                     IntToPolyDict &computed_qN, IntToPolyDict &computed_Li2qN,
                     const std::vector<IntToPolyDict> &previous_qN,
                     const std::vector<IntVector> &previous_qN_ind,
                     const std::vector<Polynomial> &expalpha_pos,
                     const std::vector<Polynomial> &expalpha_neg,
                     const IntMatrix &curves, const IntVector &degs,
                     const VecToIntDict &curve_dict, int prec, int &curr_task,
                     std::mutex &task_mut, std::mutex &m, bool computeGW,
                     int min_mem) {
  mpfr::mpreal::set_default_prec(prec);
  int h11 = curves[0].size();
  Polynomial closest_curve, tmp_qN, tmp_Li2qN, tmp_poly;
  IntVector closest_curve_diff(h11), tmp_curve_diff(h11);
  double closest_dist, tmp_dist;
  VecToIntDict::const_iterator search;
  int i, i_ind;
  while (true) {
    task_mut.lock();
    i_ind = curr_task;
    curr_task++;
    task_mut.unlock();
    if (i_ind >= qN_to_compute.size()) {
      break;
    }
    if (GetAvailableSystemMemory() < min_mem) {
      throw std::runtime_error("Memory is running low. "
                               "Exitting to prevent crash...");
    }
    i = qN_to_compute[i_ind];
    closest_curve = Polynomial();
    closest_curve.nonzero.push_back(i);
    closest_curve.coeffs[i] = 1;
    closest_curve_diff = curves[i];
    closest_dist = 0;
    for (int j = 0; j < h11; j++) {
      if (closest_curve_diff[j]) {
        closest_dist += std::log2(std::abs(closest_curve_diff[j]))+1;
      }
    }
    // Now we check to see if there is a better starting curve
    for (int j = 0; j < previous_qN_ind.size(); j++) {
      for (auto k : previous_qN_ind[j]) {
        tmp_dist = 0;
        for (int h = 0; h < h11; h++) {
          tmp_curve_diff[h] =  curves[i][h] - curves[k][h];
          if (tmp_curve_diff[h]) {
            tmp_dist += std::log2(std::abs(tmp_curve_diff[h]))+1;
          }
        }
        if (tmp_dist < closest_dist) {
          search = curve_dict.find(tmp_curve_diff);
          if (search == curve_dict.end()) {
            continue;
          }
          tmp_poly = Polynomial();
          tmp_poly.nonzero.push_back(search->second);
          tmp_poly.coeffs[search->second] = 1;
          closest_curve = PolyProd(previous_qN[j].at(k), tmp_poly, curves, degs, curve_dict);
          closest_dist = tmp_dist;
          closest_curve_diff = tmp_curve_diff;
        }
      }
    }
    // Now we compute qN and Li2(qN)
    tmp_qN = ComputeqN(closest_curve, closest_curve_diff, expalpha_pos,
                       expalpha_neg, curves, degs, curve_dict);
    tmp_Li2qN = (computeGW ? tmp_qN : PolyLi2(tmp_qN, curves, degs, curve_dict));
    m.lock();
    computed_qN[i] = std::move(tmp_qN);
    computed_Li2qN[i] = std::move(tmp_Li2qN);
    m.unlock();
  }
  mpfr_free_cache();
}

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

#include "computeGV.hpp"

#include <math.h>

#include <map>
#include <random>
#include <thread>
#include <numeric>
#include <iostream>
#include <algorithm>
#include <unordered_map>
#include <unordered_set>

// Sample input for the following function (for h11=3 example).
// {{-1, -1, 0}, {1, 0, 1}, {1, 2, 0}}
// {}
// {-3, 2, 4}
// {{1, 0, 0, -1, -1, -1, -2}, {0, 1, 0, -1, 0, 1, 1}, {0, 0, 1, 1, 1, 1, 0}}
// {}
// {{0, 0, 0, 1}, {0, 0, 1, -1}, {0, 1, 1, -1}, {1, 1, 1, -2}, {1, 1, 2, 1}, {1, 2, 2, 3}, {2, 2, 2, 3}}
// {20, 150, 0, 300000}

bool computeGVverboseMode = false;

void addCurveAndGV(CurveAndGVCollection &collection,
                   const std::vector<int> &curve,
                   mpfr_srcptr gv)
{
  mpz_class val;
  mpfr_get_z(val.get_mpz_t(), gv, MPFR_RNDN);
  collection.push_back({curve, std::move(val)});
}

void readArguments(
    int argc,
    char** argv,
    std::vector<std::vector<int>>& input_curves,
    std::vector<std::vector<int>>& lightcone_curves,
    std::vector<int>& grading_vec,
    std::vector<std::vector<int>>& Q,  // GLSM charge matrix
    VectorList& nef_partition,
    std::vector<std::vector<int>>& intnums_list,  // intersection numbers
    std::vector<int>& input_settings)             // computation settings
{
  std::cerr << "Reading input curves..." << std::endl;
  input_curves = ReadVectors(std::cin);
  if (!input_curves.size()) {
    throw std::invalid_argument("Empty curve list.");
  }

  std::cerr << "Reading curves whose past light cone will be computed...";
  std::cerr << std::endl;
  lightcone_curves = ReadVectors(std::cin);

  std::cerr << "Reading grading vector..." << std::endl;
  grading_vec = ReadVector(std::cin);

  std::cerr << "Reading GLSM charge matrix..." << std::endl;
  Q = ReadVectors(std::cin);
  if (!Q.size()) {
    throw std::invalid_argument("Empty GLSM charge matrix.");
  }

  std::cerr << "Reading nef-partition..." << std::endl;
  nef_partition = ReadVectors(std::cin, true);

  // Then we read the intersection numbers
  // Note that when dim(CY) > 3 the first index corresponds to a H_{2,2} class
  std::cerr << "Reading intersection numbers..." << std::endl;
  intnums_list = ReadVectors(std::cin);
  if (!intnums_list.size()) {
    throw std::invalid_argument("Empty intersection numbers array.");
  }

  std::cerr << "Reading computation settings..." << std::endl;
  input_settings = ReadVector(std::cin);
  if (input_settings.size() != 4) {
    throw std::invalid_argument("The input vector must have exactly three "
                                "entries: maximum degree, number of decimal"
                                "digits, mode, and minimum memory.");
  }
}


int gvcompute(
    std::vector<std::vector<int>> input_curves,
    std::vector<std::vector<int>> lightcone_curves,
    std::vector<int> grading_vec,
    std::vector<std::vector<int>> Q,  // GLSM charge matrix
    VectorList nef_partition,
    std::vector<std::vector<int>> intnums_list,  // intersection numbers
              std::vector<int> input_settings,
    CurveAndGVCollection& result)             // computation settings
{
  if (!input_curves.size()) {
    throw std::invalid_argument("Empty curve list.");
  }
  int h11 = input_curves[0].size();

  if (lightcone_curves.size() && lightcone_curves[0].size() != h11) {
    throw std::invalid_argument("Dimension mismatch.");
  }

  if (h11 != grading_vec.size()) {
    throw std::invalid_argument("Rank of lattice must match size of grading "
                                "vector.");
  }

  if (!Q.size()) {
    throw std::invalid_argument("Empty GLSM charge matrix.");
  }

  // Find the dimension and codimension of the CY
  int ambient_dim = Q[0].size() - Q.size();
  int cy_codim = (nef_partition.size() ? nef_partition.size() : 1);
  int cy_dim = ambient_dim - cy_codim;
  if (cy_dim < 3) {
    throw std::invalid_argument("CY must have dimension at least 3.");
  }

  if (!intnums_list.size()) {
    throw std::invalid_argument("Empty intersection numbers array.");
  }
  if (intnums_list[0].size() != 4) {
    throw std::invalid_argument("Intersection numbers array must have exactly "
                                "4 columns.");
  }
  VecToIntDict intnums;
  VectorSet beta_pairs;
  std::vector<int> tmp_vec_3(3);
  int min_h11_ind=0, max_h11_ind=0, min_h22_ind=0, max_h22_ind=0;
  for (auto &v : intnums_list) {
    tmp_vec_3 = {v[0], v[1], v[2]};
    int intnum = v[3];
    std::sort(tmp_vec_3.begin()+(cy_dim > 3 ? 1 : 0), tmp_vec_3.end());
    if (cy_dim == 3) {
      beta_pairs.insert({tmp_vec_3[0],tmp_vec_3[1]});
      beta_pairs.insert({tmp_vec_3[0],tmp_vec_3[2]});
      beta_pairs.insert({tmp_vec_3[1],tmp_vec_3[2]});
    }
    else {
      beta_pairs.insert({tmp_vec_3[1],tmp_vec_3[2]});
    }
    intnums[tmp_vec_3] = intnum;
    if (v[2] > max_h11_ind) {
      max_h11_ind = v[2];
    }
    if (v[1] < min_h11_ind) {
      min_h11_ind = v[1];
    }
    if (v[0] > max_h22_ind) {
      max_h22_ind = v[0];
    }
    if (v[0] < min_h22_ind) {
      min_h11_ind = v[0];
    }
  }
  intnums_list.clear();
  // Check some basic things
  if (min_h11_ind < 0 || min_h22_ind < 0) {
    throw std::invalid_argument("Indices must be non-negative.");
  }
  if (max_h11_ind >= h11) {
    throw std::invalid_argument("Indices cannot exceed h11-1.");
  }
  int h22 = (cy_dim > 3 ? max_h22_ind+1 : h11);

  // Finally, we read the maximum degree desired, the number of decimal digits,
  // the mode used for calculations, and the minimum available memory below
  // which the computation will be terminated.
  // Modes:
  //   - 0 Normal:   The full set of curves up to the specified degree is
  //                 constructed, and the backward light cone is computed.
  //   - 1 Hilbert:  Only GV invariants of Hilber basis elements are computed.
  //   - 2 Verbatim: The set of input curves are used exactly as given.
  // If the mode has the third bit set then it means that GW invariants will
  // be computed instead of GV invariants.
  if (input_settings.size() != 4) {
    throw std::invalid_argument("The input vector must have exactly three "
                                "entries: maximum degree, number of decimal"
                                "digits, mode, and minimum memory.");
  }
  int max_deg = input_settings[0];
  int digits = input_settings[1];
  int mode = input_settings[2];
  int min_mem = input_settings[3];
  input_settings.clear();

  bool computeGW = mode & 0b100;
  mode &= 0b11;

  int min_points = (max_deg >= 0 ? 0 : -max_deg);
  max_deg *= (max_deg > 0 ? 1 : 0);
  if (mode == 2) {
    min_points = 0;
  }

  if (mode < 0 || mode > 2) {
    throw std::invalid_argument("Mode must be between 0 and 2.");
  }

  MPFloat zero_cutoff = 10;
  zero_cutoff = pow(zero_cutoff,-digits/2);

  // Set the number of decimal digits
  if (digits < 17 || digits > 2000){
    throw std::invalid_argument("Number of digits (" + std::to_string(digits)
                                + ") was likely wrong. ");
  }
  int prec = mpfr::digits2bits(digits);
  mpfr::mpreal::set_default_prec(prec);

  // Set up variables for multithreading.
  int n_threads = std::thread::hardware_concurrency();
  if (!n_threads) {
    n_threads = 1;
  }
  //std::cerr << "#threads: " << n_threads << std::endl;

  std::vector<std::thread> thread_vec;
  std::mutex task_mut, mut0, mut1, mut2;
  int curr_task;

  // Now we check for nonpositive degrees
  if (computeGVverboseMode) std::cerr << "Checking consistency of input of curves..." << std::endl;
  std::unordered_set<std::vector<int>,VectorHash> curves_set(
                                                      input_curves.begin(),
                                                      input_curves.end());
  // Add the curves whose light cones will be used (in case they are not
  // already included)
  for (auto &v : lightcone_curves) {
    curves_set.insert(v);
  }
  std::vector<std::vector<int> > tmp_all_curves, tmp_new_curves, tmp_check_curves;
  std::vector<int> origin(h11, 0);
  int tmp_deg, tmp_n_curves;
  bool has_nonpos_degs = false;
  bool find_max_deg = max_deg == 0;
  // We first remove the origin to save some time
  auto origin_it = curves_set.find(origin);
  if (origin_it != curves_set.end()) {
    curves_set.erase(origin_it);
  }
  // First find the maximum degree in the list, and check for non-positive
  // degrees.
  std::vector<std::vector<int> > curves_to_remove;
  for (auto &v : curves_set) {
    tmp_deg = 0;
    for (int i = 0; i < h11; i++) {
      tmp_deg += v[i] * grading_vec[i];
    }
    if (tmp_deg <= 0) {
      has_nonpos_degs = true;
      break;
    }
    else if (tmp_deg > max_deg) {
      if (find_max_deg) {
        max_deg = tmp_deg;
      }
      else {
        curves_to_remove.push_back(v);
      }
    }
  }
  if (has_nonpos_degs) {
    throw std::invalid_argument("Non-zero curves with non-positive degrees "
                                "were found.");
  }
  for (auto &v : curves_to_remove) {
    curves_set.erase(v);
  }
  curves_to_remove.clear();
  // Remove curves from lightcone_curves if necessary.
  for (auto v = lightcone_curves.begin(); v != lightcone_curves.end(); ) {
    auto search = curves_set.find(*v);
    if (search == curves_set.end()) {
      v = lightcone_curves.erase(v);
    }
    else{
      v++;
    }
  }
  // If the maximum degree was not specified, and there are curves whose past
  // light cone will be computed, then we first set the maximum degree to half
  // of the maximum degree of these curves
  int max_lightcone_deg = 0;
  if (find_max_deg) {
    for (auto &v : lightcone_curves) {
      tmp_deg = 0;
      for (int i = 0; i < h11; i++) {
        tmp_deg += v[i] * grading_vec[i];
      }
      if (tmp_deg > max_lightcone_deg) {
        max_lightcone_deg = tmp_deg;
      }
    }
    if (max_lightcone_deg) {
      max_deg = std::ceil(float(max_lightcone_deg)/2.);
    }
  }
  // Now we find the Hilbert basis of the Mori cone
  VectorSet hilbert_set;
  if (mode != 2) {
    if (computeGVverboseMode) std::cerr << "Finding Hilbert basis..." << std::endl;
    tmp_all_curves = std::vector<std::vector<int> >(curves_set.begin(),
                                                    curves_set.end());
    tmp_n_curves = tmp_all_curves.size();
    hilbert_set = curves_set;
    curr_task = 0;
    IntMatrix vecs_to_remove;
    for (int i = 0; i < n_threads; i++) {
      thread_vec.push_back(std::thread(RemoveNonGensThr,
                           std::ref(hilbert_set), std::ref(tmp_all_curves),
                           std::ref(vecs_to_remove), std::ref(grading_vec),
                           max_deg, std::ref(curr_task), tmp_n_curves,
                           std::ref(mut0), std::ref(task_mut)));
    }
    for (auto &t : thread_vec) {
      t.join();
    }
    thread_vec.clear();
    tmp_all_curves.clear();
    for (IntVector &v : vecs_to_remove) {
      hilbert_set.erase(v);
    }
  }

  IntMatrix hilbert_basis = std::vector<std::vector<int> >(hilbert_set.begin(),
                                                           hilbert_set.end());
  // Now iterate over possible sums to find missing vectors if in normal mode
  if (min_points > 0) {
    curves_set.clear();
    curves_set.insert(origin);
    max_deg = 1;
  }
  tmp_check_curves = std::vector<std::vector<int> >(curves_set.begin(),
                                                    curves_set.end());
  tmp_n_curves = tmp_check_curves.size();
  while (mode==0) {
    curr_task = 0;
    for (int i = 0; i < (n_threads <= 4 ? n_threads : 4); i++) {
      thread_vec.push_back(std::thread(CheckCompletenessThr,
                           std::ref(curves_set), std::ref(tmp_check_curves),
                           std::ref(tmp_new_curves), std::ref(hilbert_basis),
                           std::ref(grading_vec), max_deg, std::ref(curr_task),
                           tmp_n_curves, std::ref(mut0), std::ref(task_mut),
                           min_mem));
    }
    for (auto &t : thread_vec) {
      t.join();
    }
    thread_vec.clear();
    if (tmp_new_curves.size() == 0) {
      if (curves_set.size() > min_points) {
        tmp_check_curves.clear();
        break;
      }
      max_deg++;
      continue;
    }
    if (computeGVverboseMode) std::cerr << "Found " << tmp_new_curves.size() << " new vectors" << std::endl;
    if (min_points > 0) {
      tmp_check_curves.insert(tmp_check_curves.end(), tmp_new_curves.begin(),
                              tmp_new_curves.end());
    }
    else {
      tmp_check_curves.clear();
      tmp_check_curves = std::move(tmp_new_curves);
    }
    tmp_new_curves.clear();
    tmp_n_curves = tmp_check_curves.size();
  }
  tmp_check_curves.clear();
  tmp_new_curves.clear();
  // If in Hilbert mode we only keep the Hilbert basis
  if (mode==1) {
    curves_set.clear();
    curves_set = hilbert_set;
  }
  // Finally, we add back the origin
  curves_set.insert(origin);
  // Now we construct the past light cones for the necessary curves
  std::unordered_set<std::vector<int>,VectorHash> lightcone_curves_set;
  for (unsigned int i = 0; i < lightcone_curves.size(); i++) {
    thread_vec.push_back(std::thread(HalfPastLightConeThr,
                         std::ref(curves_set), std::ref(lightcone_curves_set),
                         std::ref(lightcone_curves[i]), std::ref(mut0)));
  }
  for (auto &t : thread_vec) {
    t.join();
  }
  thread_vec.clear();
  // Now we fill out the portion of the past light cone that should be filled.
  tmp_check_curves = std::vector<std::vector<int> >(curves_set.begin(),
                                                    curves_set.end());
  tmp_n_curves = tmp_check_curves.size();
  while (mode==0) {
    curr_task = 0;
    for (int i = 0; i < (n_threads <= 4 ? n_threads : 4); i++) {
      thread_vec.push_back(std::thread(FillPastLightConeThr,
                           std::ref(curves_set), std::ref(tmp_check_curves),
                           std::ref(lightcone_curves_set),
                           std::ref(tmp_new_curves), std::ref(hilbert_basis),
                           std::ref(grading_vec), max_deg, std::ref(curr_task),
                           tmp_n_curves, std::ref(mut0), std::ref(task_mut),
                           min_mem));
    }
    for (auto &t : thread_vec) {
      t.join();
    }
    thread_vec.clear();
    if (tmp_new_curves.size() == 0) {
      tmp_check_curves.clear();
      break;
    }
    if (computeGVverboseMode) std::cerr << "Found new vectors" << std::endl;
    tmp_check_curves.clear();
    tmp_check_curves = std::move(tmp_new_curves);
    tmp_new_curves.clear();
    tmp_n_curves = tmp_check_curves.size();
    if (computeGVverboseMode) std::cerr << "Found " << tmp_check_curves.size() << " new vectors" << std::endl;
  }
  tmp_check_curves.clear();
  tmp_new_curves.clear();
  // Now we remove curves that are not in the past light cones of the selected
  // curves.
  VectorSet final_curves_set;
  for (unsigned int i = 0; i < lightcone_curves.size(); i++) {
    thread_vec.push_back(std::thread(TrimPastLightConeThr,
                         std::ref(curves_set), std::ref(final_curves_set),
                         std::ref(lightcone_curves[i]), std::ref(mut0)));
  }
  for (auto &t : thread_vec) {
    t.join();
  }
  thread_vec.clear();
  if (!final_curves_set.size()) {
    final_curves_set = std::move(curves_set);
  }
  curves_set.clear();
  input_curves.clear();
  input_curves.reserve(final_curves_set.size());
  for (auto it = final_curves_set.begin(); it != final_curves_set.end(); ) {
    input_curves.push_back(std::move(final_curves_set.extract(it++).value()));
  }
  curves_set.clear();
  int n_curves = input_curves.size();
  if (computeGVverboseMode) std::cerr << "Using " << n_curves << " curves." << std::endl;
  if (find_max_deg && max_lightcone_deg) {
    max_deg = max_lightcone_deg;
  }

  // Compute the degrees of all monomials
  IntVector degs(n_curves);
  for (int i = 0; i < n_curves; i++) {
    tmp_deg = 0;
    for (int j = 0; j < h11; j++) {
      tmp_deg += input_curves[i][j] * grading_vec[j];
    }
    degs[i] = tmp_deg;
  }
  std::vector<int> sorted_ind(n_curves);
  std::iota(sorted_ind.begin(), sorted_ind.end(), 0);
  std::sort(sorted_ind.begin(), sorted_ind.end(),
            [&degs](int a, int b) {
              return degs[a] < degs[b];
            });
  IntMatrix curves;
  curves.reserve(n_curves);
  IntVector old_degs = degs;
  for (int i = 0; i < n_curves; i++) {
    curves.push_back(input_curves[sorted_ind[i]]);
    degs[i] = old_degs[sorted_ind[i]];
  }
  input_curves.clear();
  old_degs.clear();

  // Construct a monomial/curve dictionary
  if (computeGVverboseMode) std::cerr << "Constructing monomial dictionary..." << std::endl;
  VecToIntDict curve_dict;
  for (int i = 0; i < n_curves; i++) {
    curve_dict[curves[i]] = i;
  }

  // We store some constants that will be used for polygamma computations
  MPFloat neg_em = -mpfr::const_euler();
  MPFloat pi2d6 = mpfr::const_pi()*mpfr::const_pi()/6;

  // Now we compute c and its derivatives
  if (computeGVverboseMode) std::cerr << "Computing c..." << std::endl;
  Polynomial c0;
  std::vector<Polynomial> c1(h11);
  VecToPolyDict c2;
  for (auto &pp : beta_pairs) {
    c2[pp] = Polynomial();
  }
  int h11pd = h11 + ambient_dim;
  std::vector<IntVector> Q0s(cy_codim, IntVector(h11));
  for (int i = 0; i < h11; i++) {
    for (int j = 0; j < cy_codim; j++) {
      if (!nef_partition.size()) {
        Q0s[0][i] = 0;
        for (int k = 0; k < h11pd; k++) {
          Q0s[0][i] += Q[i][k];
        }
      }
      else {
        Q0s[j][i] = 0;
        for (int k = 0; k < nef_partition[j].size(); k++) {
          Q0s[j][i] += Q[i][nef_partition[j][k]];
        }
      }
    }
  }
  IntVector tmp_vec_h11pd(h11pd);
  IntMatrix curves_dot_Q;
  for (int i = 0; i < n_curves; i++) {
    for (int j = 0; j < h11pd; j++) {
      tmp_vec_h11pd[j] = 0;
      for (int k = 0; k < h11; k++) {
        tmp_vec_h11pd[j] += curves[i][k] * Q[k][j];
      }
    }
    curves_dot_Q.push_back(tmp_vec_h11pd);
  }
  IntVector tmp_vec_ncurves(n_curves);
  IntMatrix curves_dot_Q0s;
  for (int i = 0; i < cy_codim; i++) {
    for (int j = 0; j < n_curves; j++) {
      tmp_vec_ncurves[j] = 0;
      for (int k = 0; k < h11; k++) {
        tmp_vec_ncurves[j] += curves[j][k] * Q0s[i][k];
      }
    }
    curves_dot_Q0s.push_back(tmp_vec_ncurves);
  }
  std::vector<int> neg0;
  std::vector<std::tuple<int,int> > neg1;
  std::vector<std::tuple<int,int,int> > neg2;
  int k0, k1, k2;
  for (int i = 0; i < n_curves; i++) {
    k0 = k1 = k2 = -1;
    for (int j = 0; j < h11pd; j++) {
      if (curves_dot_Q[i][j] >= 0) {
        continue;
      }
      else if (k0 == -1) {
        k0 = j;
        continue;
      }
      else if (k1 == -1) {
        k1 = j;
        continue;
      }
      k2 = j;
      break;
    }
    if (k2 != -1) {
      continue;
    }
    else if (k1 != -1) {
      neg2.push_back({i,k0,k1});
    }
    else if (k0 != -1) {
      neg1.push_back({i,k0});
    }
    else{
      neg0.push_back(i);
    }
  }

  // 0 negative
  curr_task = 0;
  for (int i = 0; i < n_threads; i++) {
    thread_vec.push_back(std::thread(computeC_0neg, std::ref(c0), std::ref(c1),
                        std::ref(c2), std::ref(neg0), std::ref(Q),
                        std::ref(Q0s), std::ref(curves_dot_Q),
                        std::ref(curves_dot_Q0s), prec, std::ref(mut0),
                        std::ref(mut1), std::ref(mut2),
                        h11, h11pd, min_mem, std::ref(curr_task),
                        std::ref(task_mut), std::ref(beta_pairs),
                        std::ref(neg_em), std::ref(pi2d6)));
  }
  for (auto &t : thread_vec) {
    t.join();
  }
  thread_vec.clear();
  neg0.clear();
  // Start computing c0inv while we compute derivatives
  PolyCleanUp(c0, zero_cutoff, true);
  Polynomial c0inv;
  std::thread c0inv_thr = std::thread(ComputeInvC0Thr, std::ref(c0inv),
                                      std::ref(c0), std::ref(curves),
                                      std::ref(degs), std::ref(curve_dict),
                                      prec);
  // 1 negative
  curr_task = 0;
  for (int i = 0; i < n_threads; i++) {
    thread_vec.push_back(std::thread(computeC_1neg, std::ref(c1), std::ref(c2),
                        std::ref(neg1), std::ref(Q), std::ref(Q0s),
                        std::ref(curves_dot_Q), std::ref(curves_dot_Q0s), prec,
                        std::ref(mut1), std::ref(mut2),
                        h11, h11pd, min_mem, std::ref(curr_task),
                        std::ref(task_mut), std::ref(beta_pairs),
                        std::ref(neg_em)));
  }
  for (auto &t : thread_vec) {
    t.join();
  }
  thread_vec.clear();
  neg1.clear();
  for (auto &p : c1) {
    PolyCleanUp(p, zero_cutoff, true);
  }
  // 2 negative
  curr_task = 0;
  for (int i = 0; i < n_threads; i++) {
    thread_vec.push_back(std::thread(computeC_2neg, std::ref(c2),
                        std::ref(neg2), std::ref(Q), std::ref(Q0s),
                        std::ref(curves_dot_Q), std::ref(curves_dot_Q0s), prec,
                        std::ref(mut2), h11, h11pd, min_mem,
                        std::ref(curr_task), std::ref(task_mut),
                        std::ref(beta_pairs)));
  }
  for (auto &t : thread_vec) {
    t.join();
  }
  thread_vec.clear();
  neg2.clear();
  for (auto &pp : beta_pairs) {
    PolyCleanUp(c2[pp], zero_cutoff, true);
  }
  // Wait for c0inv to be done
  c0inv_thr.join();
  c0 = Polynomial();

  if (computeGVverboseMode) std::cerr << "Computing alpha polynomials..." << std::endl;
  std::vector<Polynomial> alpha(h11);
  curr_task = 0;
  for (int i = 0; i < n_threads; i++) {
    thread_vec.push_back(std::thread(ComputeAlphaThr, std::ref(alpha),
                         std::ref(c0inv), std::ref(c1), std::ref(curves),
                         std::ref(degs), std::ref(curve_dict), prec,
                         std::ref(curr_task), std::ref(task_mut),
                         std::ref(mut0), min_mem));
  }
  for (auto &t : thread_vec) {
    t.join();
  }
  thread_vec.clear();
  c1.clear();
  if (computeGVverboseMode) std::cerr << "Computing beta polynomials..." << std::endl;
  VecToPolyDict beta;
  VectorList beta_pairs_vec = VectorList(beta_pairs.begin(),beta_pairs.end());
  curr_task = 0;
  for (int i = 0; i < n_threads; i++) {
    thread_vec.push_back(std::thread(ComputeBetaThr, std::ref(beta),
                         std::ref(c0inv), std::ref(c2), std::ref(curves),
                         std::ref(degs), std::ref(curve_dict), prec,
                         std::ref(curr_task), std::ref(beta_pairs_vec),
                         std::ref(task_mut), std::ref(mut0), min_mem));
  }
  for (auto &t : thread_vec) {
    t.join();
  }
  thread_vec.clear();
  c2.clear();

  // Compute F polynomials (F_ab = beta_ab - alpha_a * alpha_b)
  if (computeGVverboseMode) std::cerr << "Computing F polynomials..." << std::endl;
  VecToPolyDict F;
  curr_task = 0;
  for (int i = 0; i < n_threads; i++) {
    thread_vec.push_back(std::thread(ComputeFThr, std::ref(F),
                         std::ref(alpha), std::ref(beta), std::ref(curves),
                         std::ref(degs), std::ref(curve_dict), prec,
                         std::ref(curr_task), std::ref(beta_pairs_vec),
                         std::ref(task_mut), std::ref(mut0), min_mem));
  }
  for (auto &t : thread_vec) {
    t.join();
  }
  thread_vec.clear();
  beta.clear();
  for (auto &p : F) {
    PolyCleanUp(p.second, zero_cutoff);
  }

  // Compute instanton corrections
  if (computeGVverboseMode) std::cerr << "Computing instanton corrections..." << std::endl;
  if (cy_dim == 3) {
    h22 = h11;
  }
  std::vector<Polynomial> inst(h22);
  curr_task = 0;
  for (int i = 0; i < n_threads; i++) {
    thread_vec.push_back(std::thread(ComputeInstThr, std::ref(inst),
                        std::ref(F), std::ref(intnums), std::ref(curves),
                        std::ref(degs), std::ref(curve_dict),
                        prec, std::ref(curr_task), std::ref(task_mut),
                        std::ref(mut0), h22, cy_dim, min_mem));
  }
  for (auto &t : thread_vec) {
    t.join();
  }
  thread_vec.clear();
  F.clear();
  // Clean up inst
  for (auto &p : inst) {
    PolyCleanUp(p, zero_cutoff);
  }

  // Compute expalpha
  if (computeGVverboseMode) std::cerr << "Computing exp(alpha)..." << std::endl;
  std::vector<Polynomial> expalpha_pos(h11), expalpha_neg(h11);
  curr_task = 0;
  for (int i = 0; i < n_threads; i++) {
    thread_vec.push_back(std::thread(ComputeExpAlphaThr,
                         std::ref(expalpha_pos), std::ref(expalpha_neg),
                         std::ref(alpha), std::ref(curves), std::ref(degs),
                         std::ref(curve_dict), prec, std::ref(curr_task),
                         std::ref(task_mut), std::ref(mut0), min_mem));
  }
  for (auto &t : thread_vec) {
    t.join();
  }
  thread_vec.clear();
  alpha.clear();

  // Compute GV invariants iteratively
  if (computeGVverboseMode) std::cerr << "Computing GV..." << std::endl;
  if (computeGVverboseMode) std::cout.precision(digits);
  Polynomial tmp_poly;
  std::vector<int> vec_deg;
  MPFloat tmp_GV, tmp_GV_rounded;
  int n_previous_levels = (h11 < 4 ? 2 : (h11 < 10 ? 5 : 10));
  std::vector<IntToPolyDict> previous_qN(n_previous_levels);
  std::vector<IntVector> previous_qN_ind(n_previous_levels);
  std::vector<int> qN_to_compute;
  std::vector<MPFloat> GV_qN_to_compute;
  IntToH22GVDict h22GV_qN_to_compute;
  IntToPolyDict computed_qN, computed_Li2qN;
  for (int i = 1; i < degs[n_curves-1]+1 ; i++) {
    // First check if there are any points with the degree of interest
    vec_deg.clear();
    qN_to_compute.clear();
    GV_qN_to_compute.clear();
    h22GV_qN_to_compute.clear();
    for (int j = 0; j < n_curves; j++) {
      if (degs[j] == i) {
        vec_deg.push_back(j);
      }
      else if (degs[j] > i) {
        break;
      }
    }
    if (!vec_deg.size()) {
      continue;
    }
    if (cy_dim == 3) {
      for (auto j : vec_deg) {
        int kk = 0;
        for (int k = 0; k < h11; k++) {
          if (curves[j][k]) {
            kk = k;
            break;
          }
        }
        auto search = inst[kk].coeffs.find(j);
        if (search == inst[kk].coeffs.end()) {
          continue;
        }
        tmp_GV = search->second/curves[j][kk];
        if (computeGW) {
          if (mpfr::abs(tmp_GV) < zero_cutoff) {
            continue;
          }

          addCurveAndGV(result, curves[j], tmp_GV.mpfr_srcptr());

          if (computeGVverboseMode) {
            std::cout << "(";
            for (int k = 0; k < h11; k++) {
              std::cout << curves[j][k] << (k==h11-1 ? "" : ",");
            }
            std::cout << "), " << tmp_GV << std::endl;
          }

          qN_to_compute.push_back(j);
          GV_qN_to_compute.push_back(tmp_GV);
        }
        else {
          tmp_GV_rounded = mpfr::round(tmp_GV);
          if (abs(tmp_GV-tmp_GV_rounded) > 1e-3) {
            std::cerr << "Error: Non-integer GV invariant was found.\n(";
            for (int h = 0; h < h11; h++) {
              std::cerr << curves[j][h] << (h==h11-1 ? "" : ",");
            }
            std::cerr << "), " << tmp_GV << std::endl;
            throw std::logic_error("Non-integer GV invariant was found. Input "
                                   "may be inconsistent, or the precision "
                                   "needs to be increased.");
          }
          if (mpfr::abs(tmp_GV_rounded) < 0.5) {
            continue;
          }

          addCurveAndGV(result, curves[j], tmp_GV.mpfr_srcptr());

          if (computeGVverboseMode) {
            std::cout << "(";
            for (int k = 0; k < h11; k++) {
              std::cout << curves[j][k] << (k==h11-1 ? "" : ",");
            }
            std::cout << "), " << tmp_GV_rounded << std::endl;
          }
          qN_to_compute.push_back(j);
          GV_qN_to_compute.push_back(tmp_GV_rounded);
        }
      }
    }
    else {
      for (auto j : vec_deg) {
        for (int k = 0; k < h22; k++) {
          auto search = inst[k].coeffs.find(j);
          if (search == inst[k].coeffs.end()) {
            continue;
          }
          tmp_GV = search->second;
          if (computeGW) {
            if (mpfr::abs(tmp_GV) < zero_cutoff) { continue; }

            addCurveAndGV(result, curves[j], tmp_GV.mpfr_srcptr());

            if (computeGVverboseMode) {
              std::cout << "(";
              for (int h = 0; h < h11; h++) {
                std::cout << curves[j][h] << (h==h11-1 ? "" : ",");
              }
              std::cout << "), " << k << ", " << tmp_GV << std::endl;
            }
            auto search_h22 = h22GV_qN_to_compute.find(j);
            if (search_h22 == h22GV_qN_to_compute.end()) {
              qN_to_compute.push_back(j);
              h22GV_qN_to_compute[j] = {{k,tmp_GV}};
            }
            else {
              search_h22->second.push_back({k,tmp_GV});
            }
          }
          else {
            tmp_GV_rounded = mpfr::round(tmp_GV);
            if (abs(tmp_GV-tmp_GV_rounded) > 1e-3) {
              std::cerr << "Error: Non-integer GV invariant was found.\n(";
              for (int h = 0; h < h11; h++) {
                std::cerr << curves[j][h] << (h==h11-1 ? "" : ",");
              }
              std::cerr << "), " << k << ", " << tmp_GV << std::endl;
              throw std::logic_error("Non-integer GV invariant was found. Input "
                                     "may be inconsistent, or the precision "
                                     "needs to be increased.");
            }
            if (mpfr::abs(tmp_GV_rounded) < 0.5) {
              continue;
            }

            addCurveAndGV(result, curves[j], tmp_GV.mpfr_srcptr());

            if (computeGVverboseMode) {
              std::cout << "(";
              for (int h = 0; h < h11; h++) {
                std::cout << curves[j][h] << (h==h11-1 ? "" : ",");
              }
              std::cout << "), " << k << ", " << tmp_GV_rounded << std::endl;
            }
            auto search_h22 = h22GV_qN_to_compute.find(j);
            if (search_h22 == h22GV_qN_to_compute.end()) {
              qN_to_compute.push_back(j);
              h22GV_qN_to_compute[j] = {{k,tmp_GV_rounded}};
            }
            else {
              search_h22->second.push_back({k,tmp_GV_rounded});
            }
          }
        }
      }
    }
    if (!qN_to_compute.size()) {
      continue;
    }
    computed_qN.clear();
    computed_Li2qN.clear();
    // compute qN and Li2(qN) in parallel
    curr_task = 0;
    for (int i = 0; i < n_threads; i++) {
      thread_vec.push_back(std::thread(ComputeLi2qNThr,
                           std::ref(qN_to_compute), std::ref(computed_qN),
                           std::ref(computed_Li2qN), std::ref(previous_qN),
                           std::ref(previous_qN_ind), std::ref(expalpha_pos),
                           std::ref(expalpha_neg), std::ref(curves),
                           std::ref(degs), std::ref(curve_dict), prec,
                           std::ref(curr_task), std::ref(task_mut),
                           std::ref(mut0), computeGW, min_mem));
    }
    for (auto &t : thread_vec) {
      t.join();
    }
    thread_vec.clear();
    // Now we do the subtraction from the instanton corrections
    if (cy_dim == 3) {
      for (int j = 0; j < qN_to_compute.size(); j++) {
        for (int k = 0; k < h11; k++) {
          int jj = qN_to_compute[j];
          if (!curves[jj][k]) {
            continue;
          }
          tmp_poly = computed_Li2qN[jj];
          PolyProdScalarIP(tmp_poly, curves[jj][k]*GV_qN_to_compute[j]);
          PolySubIP(inst[k], tmp_poly);
        }
      }
    }
    else {
      for (int j = 0; j < qN_to_compute.size(); j++) {
        int jj = qN_to_compute[j];
        for (int k = 0; k < h22GV_qN_to_compute[jj].size(); k++) {
          int jj = qN_to_compute[j];
          int kk = h22GV_qN_to_compute[jj][k].first;
          tmp_GV = h22GV_qN_to_compute[jj][k].second;
          tmp_poly = computed_Li2qN[jj];
          PolyProdScalarIP(tmp_poly, tmp_GV);
          PolySubIP(inst[kk], tmp_poly);
        }
      }
    }
    for (int j = 0; j < n_previous_levels-1; j++) {
      previous_qN[j] = std::move(previous_qN[j+1]);
      previous_qN_ind[j] = std::move(previous_qN_ind[j+1]);
    }
    if (n_previous_levels > 1) {
      previous_qN[n_previous_levels-1] = std::move(computed_qN);
      previous_qN_ind[n_previous_levels-1] = std::move(qN_to_compute);
    }
  }
  return 0;
}

// int gvcompute_main(int argc, char** argv) {
//   std::vector<std::vector<int>> input_curves;
//   std::vector<std::vector<int>> lightcone_curves;
//   std::vector<int> grading_vec;
//   std::vector<std::vector<int>> Q;  // GLSM charge matrix
//   VectorList nef_partition;
//   std::vector<std::vector<int>> intnums_list;  // intersection numbers
//   std::vector<int> input_settings;             // computation settings

//   readArguments(argc,
//                 argv,
//                 input_curves,
//                 lightcone_curves,
//                 grading_vec,
//                 Q,
//                 nef_partition,
//                 intnums_list,
//                 input_settings);

//     return gvcompute(input_curves,
//                    lightcone_curves,
//                    grading_vec,
//                    Q,
//                    nef_partition,
//                    intnums_list,
//                    input_settings
//                    );            
// }

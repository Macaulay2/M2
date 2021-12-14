// Written in 2021 by Mahrud Sayrafi

#include "interface/cone.h"

#include <M2/math-include.h>

#include "debug.hpp"
#include "interface/gmp-util.h"
#include "interface/matrix.h"
#include "matrix-con.hpp"
#include "matrix.hpp"
#include "relem.hpp"

#include <libnormaliz/cone.h>
#include <vector>

typedef mpz_class Integer;

/**
 * \ingroup cones
 */

const Matrix /* or null */ *rawHilbertBasis(const Matrix *C)
{
  try
    {
      // TODO: Check that C is over ZZ
      // TODO: for cones over QQ, lift to ZZ first
      // TODO: Normaliz also supports algebraic cones defined over
      // algebraic number fields embedded in RR
      const Ring *R = C->get_ring();
      const size_t c = C->n_cols();  // rank of ambient lattice
      const size_t r = C->n_rows();  // number of cone rays

      auto rays = libnormaliz::Matrix<Integer>(r, c);
      for (size_t i = 0; i < r; i++)
        for (size_t j = 0; j < c; j++)
          rays[i][j] = static_cast<Integer>(C->elem(i, j).get_mpz());

      auto cone = libnormaliz::Cone<Integer>(libnormaliz::Type::cone, rays);
      // cone.compute(libnormaliz::ConeProperty::HilbertBasis,
      //              libnormaliz::ConeProperty::DefaultMode);
      auto HB = cone.getHilbertBasis();
      size_t n = HB.size();  // number of basis elements

      MatrixConstructor mat(R->make_FreeModule(n), c);
      for (size_t i = 0; i < n; i++)
        for (size_t j = 0; j < c; j++)
          {
            mpz_ptr z = newitem(__mpz_struct);
            mpz_init_set(z, HB[i][j].get_mpz_t());
            mpz_reallocate_limbs(z);
            mat.set_entry(i, j, ring_elem(z));
          }

      return mat.to_matrix();
  } catch (const exc::engine_error &e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

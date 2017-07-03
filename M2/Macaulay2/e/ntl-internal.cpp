// Copyright 2005, Michael Stillman

// This file contains routines which often conflict with our names

#include <cstdio>

#include <M2/config.h>
#include <M2/gc-include.h>
#include <stddef.h>
#include <gmp.h>
#include <mpfr.h>
#include "ntl-interface.hpp"

void ntl_ZZ_to_mpz(mpz_t result, const NTL::ZZ &a)
// Assumption: 'result' is already 'init'ed
// I could imagine there is a faster way to do this, but currently the only
// place
// this code is used is with NTL LLL, and even then, only to transfer elements
// to/from ntl integers
// at the beginning and at the end.
{
  if (a == 0)
    {
      mpz_set_ui(result, 0);
      return;
    }

  long len = NTL::NumBytes(a);
  unsigned char *byte_array = new unsigned char[len];
  bool is_neg = (a < 0);
  NTL::BytesFromZZ(byte_array, a, len);
  mpz_import(result, len, -1, sizeof(byte_array[0]), 0, 0, byte_array);
  delete[] byte_array;
  if (is_neg) mpz_neg(result, result);
}

NTL::ZZ ntl_ZZ_from_mpz(mpz_t a)
{
  int sgn = mpz_sgn(a);
  if (sgn == 0) return NTL::ZZ::zero();
  long len = mpz_sizeinbase(a, 8);
  unsigned char *byte_array = new unsigned char[len];
  size_t written_len;
  mpz_export(byte_array, &written_len, -1, sizeof(byte_array[0]), 0, 0, a);
  NTL::ZZ result = NTL::ZZFromBytes(byte_array, written_len);
  delete[] byte_array;
  return (sgn > 0 ? result : -result);
}

NTL::mat_ZZ *makeNTLMatrixZZ(int nrows, int ncols)
{
  NTL::mat_ZZ *X = new NTL::mat_ZZ;
  X->SetDims(nrows, ncols);
  return X;
}

void mat_ZZ_set_entry(NTL::mat_ZZ *A, long i, long j, mpz_t a)
{
  NTL::ZZ b = ntl_ZZ_from_mpz(a);
  (*A)(i + 1, j + 1) = b;
}

void mat_ZZ_get_entry(const NTL::mat_ZZ *A, long i, long j, mpz_t result)
{
  NTL::ZZ t = (*A)(i + 1, j + 1);
  ntl_ZZ_to_mpz(result, t);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:

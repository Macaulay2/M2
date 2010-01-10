#include "matrix.hpp"
#include "matrix-con.hpp"

class SymmMatrix
{
public:
  static MatrixOrNull * symmetricPower(const Matrix *m0, int p)
  {
    if (m0->n_rows() != 1)
      {
	ERROR("expected one row");
	return 0;
      }
    
    SymmMatrix s(m0,p);
    return s.value();
  }
private:
  int symm1_next;
  const Ring *R;
  int ncols;
  const Matrix *m;
  MatrixConstructor result;

  void symm1(vec f,	       // product so far generated
	     int lastn,        // can use lastn..n_cols()-1 in product
	     int pow)   // remaining power to take
  {
    if (pow == 0)
      result.set_column(symm1_next++, f);
    else
      {
	for (int i=lastn; i<ncols; i++)
	  {
	    ring_elem r = m->elem(0,i);
	    vec h = R->copy_vec(f);
	    R->mult_vec_to(h,r,false);
	    symm1(h, i, pow-1);
	  }
	R->remove_vec(f);
      }
  }
  
  SymmMatrix(const Matrix *m0, int p)
    : symm1_next(0),
      R(m0->get_ring()),
      ncols(m0->n_cols()),
      m(m0),
      result()
  {
    const FreeModule *Fp = m0->rows()->symm(p);
    const FreeModule *Gp = m0->cols()->symm(p);
    
    int *dp = R->degree_monoid()->make_new(m->degree_shift());
    R->degree_monoid()->power(dp, p, dp);
    
    result = MatrixConstructor(Fp,Gp,dp);
    
    if (p >= 0)
      {
	vec f = R->e_sub_i(0);
	symm1(f, 0, p);	  // consumes f
      }
  }
  
  Matrix * value() { return result.to_matrix(); }

};

MatrixOrNull *Matrix::symm(int n) const
{
  return SymmMatrix::symmetricPower(this,n);
}

#if 0
// 
// namespace M2 {
//   MatrixOrNull *M2::symmetricPower(const Matrix *m, int p)
//   {
//     if (m->n_rows() != 1)
//       {
// 	ERROR("expected one row");
// 	return 0;
//       }
//     
//     SymmMatrix s(m,p);
//     return s.value();
//   }
// };
// class SymmMatrix
// {
//   int symm1_next;
//   const Ring *R;
//   int ncols;
//   const Matrix *m;
//   MatrixConstructor result;
// 
// 
//   void symm1(vec f,	       // product so far generated
// 	     int lastn,        // can use lastn..n_cols()-1 in product
// 	     int pow);   // remaining power to take
// 
//   SymmMatrix(const Matrix *m, int p);
//   void compute();
//   Matrix * value();
// public:
//   friend MatrixOrNull *M2::symmetricPower(const Matrix *m, int p);
//   static MatrixOrNull * symmetricPower(const Matrix *m, int p);
// };
// 
// void SymmMatrix::symm1(vec f,           // product so far generated, consumed here
// 		       int lastn,	// can use lastn..n_cols()-1 in product
// 		       int pow)   // remaining power to take
// {
//   if (pow == 0)
//     result.set_column(symm1_next++, f);
//   else
//     {
//       for (int i=lastn; i<ncols; i++)
// 	{
// 	  ring_elem r = m->elem(0,i);
// 	  vec h = R->copy(f);
// 	  R->mult(h,r,false);
// 	  symm1(h, i, pow-1);
// 	}
//       R->remove(f);
//     }
// }
// 
// SymmMatrix::SymmMatrix(const Matrix *m0, int p)
//   : symm1_next(0),
//     R(m0->get_ring()),
//     ncols(m0->n_cols()),
//     m(m0),
//     result()
// {
//   const FreeModule *Fp = m0->rows()->symm(p);
//   const FreeModule *Gp = m0->cols()->symm(p);
// 
//   int *dp = R->degree_monoid()->make_new(m->degree_shift());
//   R->degree_monoid()->power(dp, p, dp);
//   
//   result = MatrixConstructor(Fp,Gp,dp);
// 
//   if (p >= 0)
//     {
//       vec f = R->e_sub_i(0);
//       symm1(f, 0, p);	  // consumes f
//     }
// }
// 
// Matrix * SymmMatrix::value()
// {
//   return result.to_matrix();
// }
// 
// MatrixOrNull * SymmMatrix::symmetricPower(const Matrix *m, int p)
// {
//   if (m->n_rows() != 1)
//     {
//       ERROR("expected one row");
//       return 0;
//     }
//   
//   SymmMatrix s(m,p);
//   return s.value();
// }
// 
// namespace M2 {
//   MatrixOrNull *M2::symmetricPower(const Matrix *m, int p)
//   {
//     return SymmMatrix::symmetricPower(m,p);
//   }
// };
#endif
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:

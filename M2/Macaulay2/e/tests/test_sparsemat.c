#include "engine.h"
#include "util.h"

void test_sparsemat_ZZ(void)
{
  int i,j;
  M2_arrayint C;
  MutableMatrix *Nr,*Nc;
  const Matrix *M0,*M1,*Nr1,*Nc1,*P1;
  const Ring *ZZ = IM2_Ring_ZZ();
  MutableMatrix *M = IM2_MutableMatrix_make(ZZ,20,15);
  for (i=0; i<IM2_MutableMatrix_n_rows(M); i++)
    for (j=0; j<IM2_MutableMatrix_n_cols(M); j++)
      {
	const RingElement *c = IM2_RingElement_from_Integer(ZZ, make_integer(i*i + j*j*j - j - 3));
	IM2_MutableMatrix_set_entry(M,i,j,c);
      }
  IM2_MutableMatrix_set_entry(M,2,3,IM2_RingElement_from_Integer(ZZ, make_integer(171)));
  IM2_MutableMatrix_set_entry(M,4,3,IM2_RingElement_from_Integer(ZZ, make_integer(20010)));
  
  display_sparsemat(M);
  M0 = IM2_MutableMatrix_to_matrix(M);
  Nc = IM2_MutableMatrix_iden(ZZ,IM2_MutableMatrix_n_cols(M));
  Nr = IM2_MutableMatrix_iden(ZZ,IM2_MutableMatrix_n_rows(M));
  IM2_MutableMatrix_set_col_change(M,Nc);
  IM2_MutableMatrix_set_row_change(M,Nr);
  C = IM2_FF_LU_decomp(M);
  display_sparsemat(M);
  display_sparsemat(Nr);
  display_sparsemat(Nc);
  
  for (i=0; i<C->len; i++)
    printf("%d ",C->array[i]);
  printf("\n");

  M1 = IM2_MutableMatrix_to_matrix(M);
  Nr1 = IM2_MutableMatrix_to_matrix(Nr);
  Nc1 = IM2_MutableMatrix_to_matrix(Nc);
  P1 = IM2_Matrix_mult(IM2_Matrix_mult(Nr1,M0),
		       Nc1);
  display_matrix(P1);
  assert(IM2_Matrix_is_zero(IM2_Matrix_subtract(M1,P1)));

}

int main(int argc, char **argv)
{
  IM2_initialize();
  test_sparsemat_ZZ();
  return 0;
}

#include <mpir.h>
#include <assert.h>

int main () {
  mpz_t a,b,d,u,v;
  mpz_init(a);
  mpz_init(b);
  mpz_init(d);
  mpz_init(u);
  mpz_init(v);
  mpz_set_si(a,1);
  mpz_set_si(b,2);
  mpz_gcdext(d,u,v,a,b);
  assert(!(u->_mp_size == 1 && u->_mp_d[0] == 0));
  assert(!(v->_mp_size == 1 && v->_mp_d[0] == 0));
  return 0;
}

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/libraries/mpir test && $M2BUILDDIR/libraries/mpir/test "
 End:
*/

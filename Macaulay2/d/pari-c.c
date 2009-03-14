#include <assert.h>
#include <pari/pari.h>
#include <gmp.h>
#include "M2mem.h"
#include "M2inits.h"
#include "pari-c.h"
#include "M2/config.h"

#define FALSE 0
#define TRUE 1

#ifdef HAVE_PYTHON
/* but a related issue is that when linking with python and running sage, it will tend to initialize
   the pari library itself, and we don't want to interfere with that */
#define RETAIN_PARI_STATE FALSE
#else
/* for now, we must retain the state, because pari_close_opts will use the wrong gmp memory
   allocation routines to free memory */
#define RETAIN_PARI_STATE TRUE
#endif

#define PARISIZE 1000000
#define MAXPRIME 0
#define init_flags INIT_DFTm

#if RETAIN_PARI_STATE
void initpari() __attribute__ ((constructor));
void closepari() __attribute__ ((destructor));
#define INIT 
#define CLOSE 
#else
#define INIT initpari()
#define CLOSE closepari()
#endif

static int self_initialized;

void initpari() {
  static int firsttime = TRUE;
  if (gen_0 == NULL /* && FALSE /* groan */ ) {
    pari_init_opts( PARISIZE, MAXPRIME, init_flags);
    self_initialized = TRUE;
  }
  if (firsttime) {
    firsttime = FALSE;
    assert( sizeof(*gen_0) == sizeof(mp_limb_t) ); /* our routine for direct copying does this one word at a time */
  }
  enterM2();  /* pari_init sets the memory allocation routines for gmp, so we have to set them back */
}

void closepari() {
  if (self_initialized) {
    pari_close_opts(INIT_DFTm);
    gen_0 = NULL;
    self_initialized = FALSE;
  }
}

#define varsizeof(x,n) (sizeof(*x)+(n-VARLEN)*sizeof(x->el[0]))

#if 0
/* this is necessary only if pari did not find the GNU MP library at configure time */
#define fix(m,i) ((m)-(i)-1)
#else
#define fix(m,i) i
#endif

static GEN toPari(mpz_t x) {
  int n = x->_mp_size, i;
  long m;
  long sign = 1;
  GEN z;
  if (n<0) { n=-n; sign=-1; }
  m = n+2;
  z = cgeti(m);
  setlgefint(z,m);
  setsigne(z,sign);
  for (i=0; i<n; i++) gel(z,2+fix(n,i)) = (GEN)x->_mp_d[i];
  return z;
}

static void INTtoGmp(mpz_t z, GEN y) {
  int i, m = lg(y), n = m-2;
  mpz_init2(z,8 * sizeof(*y) * n);
  for (i=0; i<n; i++) z->_mp_d[i] = (mp_limb_t)gel(y,2+fix(n,i));
  z->_mp_size = y[1] < 0 ? -n : n;
}


static mpz_col *COLtoGmp(GEN y) {
  int i, m = lg(y), n = m-1;
  mpz_col *z = (mpz_col *)getmem(varsizeof(z,n));
  z->n = n;
  for (i=0; i<n; i++) {
    z->el[i] = (__mpz_struct *)getmem(sizeof(__mpz_struct));
    INTtoGmp(z->el[i],gel(y,i+1));
  }
  return z;
}

static mpz_mat *MATtoGmp(GEN y) {
  int i, m = lg(y), n = m-1;
  mpz_mat *z = (mpz_mat *)getmem(varsizeof(z,n));
  z->n = n;
  for (i=0; i<n; i++) z->el[i] = COLtoGmp(gel(y,i+1));
  return z;
}

mpz_mat *pari_factorint(mpz_t x, long flags) {
  mpz_mat *f;
  {
    INIT;
    pari_sp save_stack_pointer = avma;
    f = MATtoGmp(factorint(toPari(x),flags));
    avma = save_stack_pointer;
    CLOSE;
  }
  return f;
}

bool pari_isprime(mpz_t x) {
  long f;
  {
    INIT;
    pari_sp save_stack_pointer = avma;
    f = isprime(toPari(x));
    avma = save_stack_pointer;
    CLOSE;
  }
  return f != 0;
}

bool pari_ispseudoprime(mpz_t x, long flags) {
  long f;
  {
    INIT;
    pari_sp save_stack_pointer = avma;
    f = ispseudoprime(toPari(x), flags);
    avma = save_stack_pointer;
    CLOSE;
  }
  return f != 0;
}

/*

  debugging stuff

*/

#define pari_examine(x) do { fputs(" " #x ": ", stdout); voir(x,-1); } while (0)
#define pari_display(x) do { fputs(" " #x ": ", stdout); outbeaut(x); } while (0)
#define abs(x) ((x)<0?-(x):(x))
static void gmp_examine0(mpz_t z) {
  int i, n = max(z->_mp_alloc,abs(z->_mp_size));
  printf("alloc %d size %d %s {",z->_mp_alloc,z->_mp_size,z->_mp_size < 0 ? "-" : "+");
  for (i=0; i<n; i++) printf(" %lx",(unsigned long)z->_mp_d[i]);
  printf(" }\n");
}
#define gmp_examine(x) do { fputs(" " #x ": ", stdout); gmp_examine0(x); } while (0)

static void gmp_display0(mpz_t x) {
  mpz_out_str(stdout, 10, x);
  printf("\n");
}
#define gmp_display(x) do { fputs(" " #x ": ", stdout); gmp_display0(x); } while (0)

static void testnum1(mpz_t x) {
  mpz_init(x);
  mpz_set_si(x,1L);
  mpz_mul_si(x,x,3331333L);
  mpz_mul_si(x,x,3333133L);
  mpz_mul_si(x,x,3333313L);
  mpz_mul_si(x,x,3333313L);
  mpz_mul_si(x,x,3333331L);
}

void pari_test() {
  INIT;

  mpz_t x;
  testnum1(x);
  gmp_display(x);
  gmp_examine(x);
  pari_display(toPari(x));
  pari_examine(toPari(x));

  GEN y = gen_1;
  y = gmulgs(y,3331333);
  y = gmulgs(y,3333133);
  y = gmulgs(y,3333313);
  y = gmulgs(y,3333313);
  y = gmulgs(y,3333331);
  pari_display(y);
  pari_examine(y);

  CLOSE;
}

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/

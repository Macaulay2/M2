#include <assert.h>
#include <pari/pari.h>
#include <gmp.h>
#define drop cgiv
#define repile(x,y) { pari_sp ltop = avma; x = gerepileupto(ltop, y); }
#define assign(x,y) { x = y; }
#define see(x) see0(#x,x)
static void see0(char *n, GEN z) {
  int i;
  printf(" %s: %3d %p typ %ld lg %ld {", n, (GEN)top-z, z, typ(z), lg(z));
  for (i=0; i<lg(z); i++) printf(" %p",gel(z,i));
  printf(" }\n");
}
#define Voir(x) do { fputs(" " #x ": ", stdout); voir(x,-1); } while (0)
#define beaut(x) do { fputs(" " #x ": ", stdout); outbeaut(x); } while (0)
#define abs(x) ((x)<0?-(x):(x))
// defined already in paricom.h:
// #define max(x,y) ((x)<(y)?(y):(x))
static void g0(mpz_t z) {
  int i, n = max(z->_mp_alloc,abs(z->_mp_size));
  printf("alloc %d size %d %s {",z->_mp_alloc,z->_mp_size,z->_mp_size < 0 ? "-" : "+");
  for (i=0; i<n; i++) printf(" %lx",(unsigned long)z->_mp_d[i]);
  printf(" }\n");
}
#define g(x) do { fputs(" " #x ": ", stdout); g0(x); } while (0)

static void printgmp0(mpz_t x) {
  mpz_out_str(stdout, 10, x);
  printf("\n");
}
#define printgmp(x) do { fputs(" " #x ": ", stdout); printgmp0(x); } while (0)

void makegmpnum(mpz_t x) {
  mpz_init(x);
  mpz_set_si(x,1L);
  mpz_mul_si(x,x,3331333L);
  mpz_mul_si(x,x,3333133L);
  mpz_mul_si(x,x,3333313L);
  mpz_mul_si(x,x,3333313L);
  mpz_mul_si(x,x,3333331L);
}

GEN toPari(mpz_t x) {
  int n = x->_mp_size, i;
  long m;
  long sign = 1;
  GEN z;
  if (n<0) { n=-n; sign=-1; }
  m = n+2;
  z = cgeti(m);
  setlgefint(z,m);
  setsigne(z,sign);
  for (i=0; i<n; i++) gel(z,m-i-1) = (GEN)x->_mp_d[i];
  return z;
}

static void INTtoGmp(mpz_t z, GEN y) {
  int i, m = lg(y), n = m-2, sign = y[1];
  mpz_init2(z,8 * sizeof(*y) * n);
  for (i=0; i<n; i++) z->_mp_d[i] = (mp_limb_t)gel(y,m-i-1);
  z->_mp_size = sign < 0 ? -n : n;
}

#define VARLEN 1
typedef struct { unsigned int n; __mpz_struct *el[VARLEN]; } mpz_col;
typedef struct { unsigned int n; mpz_col *el[VARLEN]; } mpz_mat;
#define varsizeof(x,n) (sizeof(*x)+(n-VARLEN)*sizeof(x->el[0]))

static void printCOL(mpz_col *q) {
  int i;
  fputs("{", stdout);
  for (i=0; i<q->n; i++) { fputs(" ",stdout); mpz_out_str(stdout, 10, q->el[i]); }
  fputs(" }", stdout);
}

static void printMAT0 (mpz_mat *q) {
  int i;
  fputs("{", stdout);
  for (i=0; i<q->n; i++) { fputs(" ",stdout); printCOL(q->el[i]); }
  fputs(" }\n", stdout);
}
#define printMAT(q) do { fputs(" " #q ": ", stdout); printMAT0(q); } while (0)

static mpz_col *COLtoGmp(GEN y) {
  int i, m = lg(y), n = m-1;
  mpz_col *z = (mpz_col *)malloc(varsizeof(z,n));
  z->n = n;
  for (i=0; i<n; i++) {
    z->el[i] = (__mpz_struct *)malloc(sizeof(__mpz_struct));
    INTtoGmp(z->el[i],gel(y,i+1));
  }
  return z;
}

static mpz_mat *MATtoGmp(GEN y) {
  int i, m = lg(y), n = m-1;
  mpz_mat *z = (mpz_mat *)malloc(varsizeof(z,n));
  z->n = n;
  for (i=0; i<n; i++) z->el[i] = COLtoGmp(gel(y,i+1));
  return z;
}

mpz_mat *factorgmp(mpz_t x) {
  pari_sp save_stack_pointer = avma;
  mpz_mat *f = MATtoGmp(factor(toPari(x)));
  avma = save_stack_pointer;
  return f;
}

void initpari() __attribute__ ((constructor));
void initpari() {pari_init(10000000, 0);}

int main (int argc, char **argv) {
  mpz_t y;
  mpz_mat *f;
  makegmpnum(y);
  f = factorgmp(y);
  printMAT(f);
  return 0;
}
/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/libraries/pari run-example "
 End:
*/

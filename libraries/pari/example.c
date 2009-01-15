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
  mpz_set_si(x,734985345L);
  mpz_mul_si(x,x,3331333L);
  mpz_mul_si(x,x,3333133L);
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

void INTtoGmp(mpz_t z, GEN y) {
  int i, m = lg(y), n = m-2, sign = y[1];
  mpz_init2(z,8 * sizeof(*y) * n);
  for (i=0; i<n; i++) z->_mp_d[i] = (mp_limb_t)gel(y,m-i-1);
  z->_mp_size = sign < 0 ? -n : n;
}

typedef struct { int n; mpz_t *el; } mpz_col;
typedef struct { int n; mpz_col *el; } mpz_mat;

void printCOL(mpz_col q) {
  int i;
  fputs("{", stdout);
  for (i=0; i<q.n; i++) { fputs(" ",stdout); mpz_out_str(stdout, 10, q.el[i]); }
  fputs(" }", stdout);
}

void printMAT (mpz_mat q) {
  int i;
  fputs("{", stdout);
  for (i=0; i<q.n; i++) { fputs(" ",stdout); printCOL(q.el[i]); }
  fputs(" }", stdout);
}

mpz_col COLtoGmp(GEN y) {
  int i, m = lg(y), n = m-1;
  mpz_col z = { n, (mpz_t *)malloc(n * sizeof(mpz_t)) };
  for (i=0; i<n; i++) INTtoGmp(z.el[i],gel(y,i+1));
  return z;
}

mpz_mat MATtoGmp(GEN y) {
  int i, m = lg(y), n = m-1;
  mpz_mat z = { n, (mpz_col *)malloc(n * sizeof(mpz_col)) };
  for (i=0; i<n; i++) z.el[i] = COLtoGmp(gel(y,i+1));
  return z;
}

void factorgmp(mpz_t z, mpz_t x) {
  pari_sp save_stack_pointer = avma;
  GEN f = factor(toPari(x));
  // ...
  avma = save_stack_pointer;
}

int main (int argc, char **argv) {
  pari_sp save_stack_pointer = avma;
  pari_init(10000000, 0);
  GEN z, w;
  mpz_t y, y2;
  mpz_mat f;
  makegmpnum(y);
  g(y);
  printgmp(y);
  z = toPari(y);
  see(z);
  beaut(z);
  w = factor(z);
  Voir(w);
  see(w);
  Voir(gel(w,1));
  see(gel(w,1));
  Voir(gel(w,2));
  see(gel(w,2));
  beaut(w);
  f = MATtoGmp(w);
  fputs(" f: ",stdout);
  printMAT(f);
  fputs("\n",stdout);
  avma = save_stack_pointer;
  return 0;
}
/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/libraries/pari run-example "
 End:
*/

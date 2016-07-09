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
#if 0
    /* when linking with python and running sage, sage will tend to initialize
       the pari library itself, and we don't want to interfere with that */
#define RETAIN_PARI_STATE FALSE
#else
    /* but for now, we must retain the state, because of a bug in pari 2.3.4 that causes pari_close_opts use a
       garbage pointer to free memory; I've reported the bug */
#define RETAIN_PARI_STATE TRUE
#endif
#else
#define RETAIN_PARI_STATE TRUE
#endif

#define PARISIZE 1000000
#define MAXPRIME 0
#define init_flags INIT_DFTm

#if RETAIN_PARI_STATE
static void initpari() __attribute__ ((constructor));
static void closepari() __attribute__ ((destructor));
#define INIT
#define CLOSE
#else
#define INIT initpari()
#define CLOSE closepari()
#endif

static int self_initialized;

static int pari_disabled;

/*
 * This function overrides the cb_pari_err_recover which is initialized to pari_exit() by default.
 * The cb_pari_err_recover function is called after PARI has cleaned-up from an error. In our case
 * it will be called by PARI with errnum = -1 after using allocatemem.
 */

static void m2_pari_err_recover(long errnum) {
  if (errnum != -1) {
    exit(1);
  }
}

static void initpari() {
  if (pari_disabled) return;
  static int firsttime = TRUE;
  if (firsttime) {
    firsttime = FALSE;
    if ( sizeof(*gen_0) != sizeof(mp_limb_t) ) {
      /* our routine for direct copying does this one word at a time */
      pari_disabled = TRUE;
      return;
    }
  }
  if (gen_0 == NULL /* && FALSE /-* groan */ ) {
    pari_init_opts( PARISIZE, MAXPRIME, init_flags);
    self_initialized = TRUE;
  }
  cb_pari_err_recover = m2_pari_err_recover;
  enterM2();  /* pari_init sets the memory allocation routines for gmp, so we have to set them back */
}

static void closepari() {
  if (pari_disabled) return;
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
  if (pari_disabled) return NULL;
  GEN z;
  int n = x->_mp_size, i;
  long m;
  long sign = 1;
  if (n<0) { n=-n; sign=-1; }
  m = n+2;
  z = cgeti(m);
  setlgefint(z,m);
  setsigne(z,sign);
  for (i=0; i<n; i++) gel(z,2+fix(n,i)) = (GEN)x->_mp_d[i];
  return z;
}

static void INTtoGmp(mpz_t z, GEN y) {
  if (pari_disabled) return;
  int i, m = lg(y), n = m-2;
  mpz_init2(z,8 * sizeof(*y) * n);
  for (i=0; i<n; i++) z->_mp_d[i] = (mp_limb_t)gel(y,2+fix(n,i));
  z->_mp_size = y[1] < 0 ? -n : n;
}


static mpz_col *COLtoGmp(GEN y) {
  if (pari_disabled) return NULL;
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
  if (pari_disabled) return NULL;
  int i, m = lg(y), n = m-1;
  mpz_mat *z = (mpz_mat *)getmem(varsizeof(z,n));
  z->n = n;
  for (i=0; i<n; i++) z->el[i] = COLtoGmp(gel(y,i+1));
  return z;
}

mpz_mat *pari_factorint(mpz_t x, long flags) {
  mpz_mat *f;
  {
    if (pari_disabled) return NULL;

    INIT;
    /* Start PARI computations. */
    pari_CATCH(e_STACK) {
#ifdef NDEBUG
      /*
       * Every time the stack is changed PARI writes a message to the file pari_errfile
       * which by default is /dev/stderr. To avoid showing this message to the user we
       * redirect to /dev/null before the PARI's stack is modified.
       */
      FILE *tmp, *dev_null = fopen("/dev/null", "w");
      if (dev_null != NULL) {
        tmp = pari_errfile;
        pari_errfile = dev_null;
      }
#endif
      allocatemem(0); // passing 0 will double the current stack size.
#ifdef NDEBUG
      /*
       * We set pari_errfile back to the default value just in case PARI crashes.
       */
      if (dev_null != NULL) {
        pari_errfile = tmp;
        fclose(dev_null);
      }
#endif
    } pari_RETRY {
      pari_sp save_stack_pointer = avma;
      f = MATtoGmp(factorint(toPari(x),flags));
      avma = save_stack_pointer;
    }
    pari_ENDCATCH
    CLOSE;
  }

  return f;
}

int pari_isprime(mpz_t x) {
  long f;
  {
    if (pari_disabled) return -1;

    INIT;
    /* Start PARI computations. */
    pari_CATCH(e_STACK) {
#ifdef NDEBUG
      /*
       * Every time the stack is changed PARI writes a message to the file pari_errfile
       * which by default is /dev/stderr. To avoid showing this message to the user we
       * redirect to /dev/null before the PARI's stack is modified.
       */
      FILE *tmp, *dev_null = fopen("/dev/null", "w");
      if (dev_null != NULL) {
        tmp = pari_errfile;
        pari_errfile = dev_null;
      }
#endif
      allocatemem(0); // passing 0 will double the current stack size.
#ifdef NDEBUG
      /*
       * We set pari_errfile back to the default value just in case PARI crashes.
       */
      if (dev_null != NULL) {
        pari_errfile = tmp;
        fclose(dev_null);
      }
#endif
    } pari_RETRY {
      pari_sp save_stack_pointer = avma;
      f = isprime(toPari(x));
      avma = save_stack_pointer;
    }
    pari_ENDCATCH
    CLOSE;
  }

  return f != 0;
}

int pari_ispseudoprime(mpz_t x, long flags) { /* used in pari.d */
  long f;
  {
    if (pari_disabled) return -1;

    INIT;
    /* Start PARI computations. */
    pari_CATCH(e_STACK) {
#ifdef NDEBUG
      /*
       * Every time the stack is changed PARI writes a message to the file pari_errfile
       * which by default is /dev/stderr. To avoid showing this message to the user we
       * redirect to /dev/null before the PARI's stack is modified.
       */
      FILE *tmp, *dev_null = fopen("/dev/null", "w");
      if (dev_null != NULL) {
        tmp = pari_errfile;
        pari_errfile = dev_null;
      }
#endif
      allocatemem(0); // passing 0 will double the current stack size.
#ifdef NDEBUG
      /*
       * We set pari_errfile back to the default value just in case PARI crashes.
       */
      if (dev_null != NULL) {
        pari_errfile = tmp;
        fclose(dev_null);
      }
#endif
    } pari_RETRY {
      pari_sp save_stack_pointer = avma;
      f = ispseudoprime(toPari(x), flags);
      avma = save_stack_pointer;
    }
    pari_ENDCATCH
    CLOSE;
  }

  return f != 0;
}

/*

  debugging stuff

*/

#define pari_examine(x) do { fputs(" " #x ": ", stdout); voir(x,-1); } while (0)
#define pari_display(x) do { fputs(" " #x ": ", stdout); output(x); } while (0)
#define abs(x) ((x)<0?-(x):(x))

#define max(a,b) ((a)>(b)?(a):(b))

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

static void testnum1(mpz_t x) {		/* to be called from the debugger */
  mpz_init(x);
  mpz_set_si(x,1L);
  mpz_mul_si(x,x,3331333L);
  mpz_mul_si(x,x,3333133L);
  mpz_mul_si(x,x,3333313L);
  mpz_mul_si(x,x,3333313L);
  mpz_mul_si(x,x,3333331L);
}

void pari_test() {
  if (pari_disabled) return;
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

// now undefine some macros defined in <pari/paricom.h> :
char *get_pari_version() {
  /*
    /usr/include/pari/paricfg.h:#define PARI_VERSION_CODE 131843
    /usr/include/pari/paricfg.h:#define PARI_VERSION(a,b,c) (((a) << 16) + ((b) << 8) + (c))
    It's disappointing that the version number of libpari.so is not available at run time.
  */
  static char buf[40];
  sprintf(buf,"%d.%d.%d%s",
	  0xff & (PARI_VERSION_CODE >> 16),
	  0xff & (PARI_VERSION_CODE >> 8),
	  0xff & (PARI_VERSION_CODE >> 0),
	  pari_disabled ? " (disabled)" : ""
	  );
  return buf;
}


/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/

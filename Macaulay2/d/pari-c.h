#define VARLEN 1
typedef struct { unsigned int n; __mpz_struct *el[VARLEN]; } mpz_col;
typedef struct { unsigned int n; mpz_col *el[VARLEN]; } mpz_mat;
extern mpz_mat *pari_factorint(mpz_t, long flags);
typedef char bool;
extern bool pari_isprime(mpz_t);

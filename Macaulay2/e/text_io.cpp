// (c) 1994 Michael E. Stillman

#include <stdio.h>
#include "text_io.hpp"

int p_plus = 0;
int p_one = 1;
int p_parens = 0;

int i_text_io()
{
  p_plus = 0;
  p_one = 1;
  p_parens = 0;
  return 1;
}

void bignum_text_out(buffer &o, mpz_t a)
{
  char s[1000];
  char *str;

  int size = mpz_sizeinbase(a, 10) + 2;
  char *allocstr = (size > 1000 ? new char[size] : s);

  str = mpz_get_str(allocstr, 10, a);
  o << str;

  if (size > 1000) delete [] allocstr;
}

void emit(char *s)
{
  fprintf(stderr, s);
}

void emit_line(char *s)
{
  fprintf(stderr, "%s%s", s, "\n");
}


// (c) 1994 Michael E. Stillman

#include "text-io.hpp"
#include "newdelete.hpp"
#include <cstdio>
#include <cstring>

int MAX_LINE_LENGTH = 75;
int emit_line_len = 0;

void bignum_text_out(buffer &o, mpz_srcptr a)
{
  char s[1000];
  char *str;

  int size = static_cast<int>(mpz_sizeinbase(a, 10) + 2);
  char *allocstr = (size > 1000 ? newarray_atomic(char, size) : s);

  str = mpz_get_str(allocstr, 10, a);
  o << str;

  if (size > 1000) freemem(allocstr);
}

void clear_emit_size() { emit_line_len = 0; }
void emit_wrapped(const char *s)
{
  // We wish verbose display to wrap at some reasonable length.
  emit_line_len -= static_cast<int>(strlen(s));
  if (emit_line_len <= 0)
    {
      emit_line_len = MAX_LINE_LENGTH;
      fprintf(stdout, "\n");
      fprintf(stdout, wrapping_prefix);
    }
  fputs(s, stdout);
  fflush(stdout);
}

void emit(const char *s)
{
  fputs(s, stdout);
  fflush(stdout);
}

void emit_line(const char *s)
{
  while (*s != 0)
    {
      while (*s != '\n' && *s != 0) putchar(*s++);
      putchar('\n');
      if (*s != 0) s++;
      fputs(wrapping_prefix, stdout);
    }
  fflush(stdout);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:

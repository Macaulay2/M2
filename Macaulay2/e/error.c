// (c) 1997 Michael E. Stillman

// This stuff would not be necessary if exceptions worked well,
// unfortunately, there are internal errors so that they fail
// at least in the presence of template code (even in version 2.7.2).
// So, we use the following simple, if unpleasant, scheme.
//
// Upon finding an error, call ERROR any number of times.  Each time,
// The message is added to the end (but, it won't write over the end of
// the buffer).
//
// Then, it is unfortunately necessary to check whether an error has
// occured, using 'error'.
//
// Finally, the error flag is cleared upon giving control back to the front end.

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

#define MAXERROR 200
static int _iserror = 0;
static char errmsg[MAXERROR] = {'\0'};

void ERROR(char *s,...)
{
  va_list ap;
  va_start(ap,s);
  if (!_iserror)
    {
      _iserror = 1;
      vsprintf(errmsg,s,ap);
    }
  va_end(ap);
}

void INTERNAL_ERROR(char *s,...)
{
  va_list ap;
  va_start(ap,s);
  vsprintf(errmsg,s,ap);
  va_end(ap);
  fprintf(stderr, "%s\n", errmsg);
  exit(1);
}

int error()
{
  return _iserror;
}

char *error_message()
{
  return errmsg;
}

void clear_error()
{
  errmsg[0] = '\0';
  _iserror = 0;
}

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
*/

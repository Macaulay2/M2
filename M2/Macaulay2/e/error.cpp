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

static const int MAXERROR = 100;
static bool iserror = false;
static char errmsg[MAXERROR] = {'\0'};
static int nextloc = 0;

void ERROR(char *s)
{
  if (!iserror)
    {
      iserror = true;
      while (nextloc < MAXERROR && *s != '\0')
	errmsg[nextloc++] = *s++;
    }
}

void ERROR(int n)
{
  char buf[MAXERROR];
  sprintf(buf, "%d", n);
  char *s = buf;
  ERROR(s);
}

bool error()
{
  return iserror;
}

char *error_message()
{
  return errmsg;
}

void clear_error()
{
  nextloc = 0;
  errmsg[0] = '\0';
  iserror = false;
}

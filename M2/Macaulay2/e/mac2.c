#define GC 1		/* temporary */

typedef struct stringrec{
#if !GC
     unsigned int refs; 
#endif
     unsigned int len;
     char arry[1];
     } *string;

extern string tostringn(char *,int);
extern void *GBgbprocess(char *instring, int inlen);

extern void *make_string(char *s, int len);
extern string GB_gbprocess(string s);

void *make_string(char *s, int len)
{
  return (void *) tostringn(s,len);
}

#if 0
void GB_gbstart(void)
{
  GBstart();
}
#endif

string GB_gbprocess(string s)
{
  string result;
  result = (string) GBgbprocess(s->arry, s->len);
  return result;
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:

-- Copyright 2010 by Daniel R. Grayson

declarations "#include <M2/config.h>";

use arithmetic;

export string := array(char);
export arrayint := array(int);
export arrayintOrNull := array(int) or null;
export stringCell := {+ v:string };
export ArrayString := array(string);
export ArrayStringOrNull := null or ArrayString;
export charstar := atomicPointer "char *";
export ucharstar := atomicPointer "unsigned char *";
export charstarstar := Pointer "char **";
export constcharstar := atomicPointer "const char *";
export constucharstar := atomicPointer "const unsigned char *";
export constcharstarstar := Pointer "const char * const *";
export charstarOrNull := charstar or null;
export constcharstarOrNull := constcharstar or null;
export constucharstarOrNull := constucharstar or null;
export constcharstarstarOrNull := constcharstarstar or null;
const(x:charstarOrNull):constcharstarOrNull := Ccode(constcharstarOrNull,x);

header "#include <M2mem.h>";
header " extern void *GC_check_annotated_obj(void*); ";
export tostring(s:constcharstarOrNull):string := -- we want the name of this function to be "tostring", sigh, so keep it first
Ccode(returns, "
  int n = s ? strlen(s) : 0;
  M2_string p = getmematomicarraytype(M2_string,n);
  p->len = n;
  memcpy(p->array,s,n);
  GC_CHECK_CLOBBER(p);
  return p;
");
export tostring(s:charstarOrNull):string := tostring(const(s)); -- make sure this one is second; teach the language about "const", sigh
export tostring(s:charstar):string := tostring(charstarOrNull(s));
export tostring(s:constcharstar):string := tostring(constcharstarOrNull(s));

export makearrayint(n:int):arrayint := Ccode(returns, "
  M2_arrayint z = (M2_arrayint)getmem_atomic(sizeofarray(z,n));
  z->len = n;
  GC_CHECK_CLOBBER(z);
  return z; /* Note that getmem_atomic returns zeroed memory */
");
export tocharstar(s:string):charstar := Ccode(returns, "
  char *p = getmem_atomic(s->len + 1);
  memcpy(p,s->array,s->len);
  p[s->len] = 0;
  GC_CHECK_CLOBBER(p);
  return p;
");
export tocharstarOrNull(s:string):charstarOrNull := Ccode(returns, "
  char *p;
  if (s->len == 0) return NULL;
  p = getmem_atomic(s->len + 1);
  memcpy(p,s->array,s->len);
  p[s->len] = 0;
  GC_CHECK_CLOBBER(p);
  return p;
");
export tostringn(s:charstar,n:int):string := Ccode(returns, "
  M2_string p = (M2_string)getmem_atomic(sizeofarray(p,n));
  p->len = n;
  memcpy(p->array,s,n);
  GC_CHECK_CLOBBER(p);
  return p;
");
export join(x:string,y:string):string := Ccode(returns, "
  M2_string p;
  p = (M2_string) getmem_atomic(sizeofarray(p,x->len+y->len));
  p->len = x->len + y->len;
  memcpy(p->array,x->array,x->len);
  memcpy(p->array+x->len,y->array,y->len);
  GC_CHECK_CLOBBER(p);
  return p;
");
export tocharstarstar(p:ArrayString):charstarstar := (
     Ccode(returns, "
	  unsigned int i, n = p->len;
	  char **s = getmemvectortype(char *,n+1);
	  for (i = 0; i<n; i++) s[i] = M2_tocharstar(p->array[i]);
	  s[n] = NULL;
	  return s;
	  "));
export tocharstarmalloc(s:string):charstar := Ccode(returns, "
  char *p = getmem_malloc(s->len + 1);
  memcpy(p,s->array,s->len);
  p[s->len] = 0;
  return p; ");
export tocharstarstarmalloc(p:ArrayString):charstarstar := Ccode(returns, "
  unsigned int n = p->len;
  char **s = (char **)getmem_malloc((n + 1)*sizeof(char *));
  unsigned int i;
  for (i=0; i<n; i++) s[i] = M2_tocharstarmalloc(p->array[i]);
  s[n] = NULL;
  return s;");
export tostrings(n:int,s:constcharstarstar):ArrayString := Ccode(returns, "
  int i;
  M2_ArrayString a = getmemarraytype(M2_ArrayString,n);
  a->len = n;
  for (i=0; i<n; i++) a->array[i] = M2_tostring(s[i]);
  GC_CHECK_CLOBBER(a);
  return a;");
export enlarge(n:int,x:string):string := Ccode(returns, "
  M2_string p;
  int m = x->len + n;
  if (n <= 0) return x;
  p = (M2_string) getmem_atomic(sizeofarray(p,m));
  p->len = m;
  memcpy(p->array,x->array,x->len);
  memset(p->array+x->len,0,n);
  GC_CHECK_CLOBBER(p);
  return p;");
export substr(x:string,start:int,leng:int):string := Ccode(returns, "
  static struct M2_string_struct emptyM2String;
  M2_string p;
  if (start < 0) start += x->len;	/* start<0 means count from the end */
  if (start < 0) leng += start, start = 0;
  if (start + leng > (int)x->len) leng = x->len - start;
  if (leng <= 0) return &emptyM2String;
  if (start == 0 && leng == (int)x->len) return x;
  p = (M2_string) getmem_atomic(sizeofarray(p,leng));
  p->len = leng;
  memcpy(p->array,x->array+start,leng);
  GC_CHECK_CLOBBER(p);
  return p;");
export substr(x:string,start:int):string := substr(x,start,length(x) - start);
export substrAlwaysCopy(x:string,start:int,leng:int):string := Ccode(returns, "
  static struct M2_string_struct emptyM2String;
  M2_string p;
  if (start < 0) start += x->len;	/* start<0 means count from the end */
  if (start < 0) leng += start, start = 0;
  if (start + leng > (int)x->len) leng = x->len - start;
  if (leng <= 0) return &emptyM2String;
  /* if (start == 0 && leng == (int)x->len) return x; */
  p = (M2_string) getmem_atomic(sizeofarray(p,leng));
  p->len = leng;
  memcpy(p->array,x->array+start,leng);
  GC_CHECK_CLOBBER(p);
  return p;");

declarations " extern char newline[]; ";
header " char newline[] = \"\\n\"; ";
export newline := tostring(Ccode(constcharstarOrNull,"newline"));
export envc := 0;
export argc := 0;
export envp := array(string)();
export argv := array(string)();
export args := array(string)();
export gbTrace := 0;
export numTBBThreads := 0; -- 0 means use the maximum
export numericalAlgebraicGeometryTrace := 0;
export notify := false;
export readonlyfiles := false;				    -- see stdio.d

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d M2.o "
-- End:

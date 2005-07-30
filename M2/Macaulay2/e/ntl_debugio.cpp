// Copyright 2005, Michael Stillman

#include <iostream>
#include "ntl_interface.hpp"

static long iodigits = 0;
static long ioradix = 0;

// iodigits is the greatest integer such that 10^{iodigits} < NTL_WSP_BOUND
// ioradix = 10^{iodigits}

static void InitZZIO()
{
   long x;

   x = (NTL_WSP_BOUND-1)/10;
   iodigits = 0;
   ioradix = 1;

   while (x) {
      x = x / 10;
      iodigits++;
      ioradix = ioradix * 10;
   }

   if (iodigits <= 0) Error("problem with I/O");
}

// The class _ZZ_local_stack should be defined in an empty namespace,
// but since I don't want to rely on namespaces, we just give it a funny 
// name to avoid accidental name clashes.

struct _localZZ_local_stack {
   long top;
   long alloc;
   long *elts;

   _localZZ_local_stack() { top = -1; alloc = 0; elts = 0; }
   ~_localZZ_local_stack() { }

   long pop() { return elts[top--]; }
   long empty() { return (top == -1); }
   void push(long x);
};

void _localZZ_local_stack::push(long x)
{
   if (alloc == 0) {
      alloc = 100;
      elts = new long[alloc];
   }

   top++;

#if 0
   if (top + 1 > alloc) {
      alloc = 2*alloc;
      elts = NTL_REALLOC(elts, alloc, sizeof(long), 0);
   }
#endif
   if (!elts) {
      Error("out of space in ZZ output");
   }

   elts[top] = x;
}

static
void PrintDigits(std::ostream& s, long d, long justify)
{
   static char *buf = 0;

   if (!buf) {
      buf = new char[iodigits];
      if (!buf) Error("out of memory");
   }

   long i = 0;

   while (d) {
      buf[i] = IntValToChar(d % 10);
      d = d / 10;
      i++;
   }

   if (justify) {
      long j = iodigits - i;
      while (j > 0) {
         s << "0";
         j--;
      }
   }

   while (i > 0) {
      i--;
      s << buf[i];
   }
}

#if 0      
std::ostream& operator<<(std::ostream& s, const ZZ& a)
{
   static ZZ b;
   static _localZZ_local_stack S;
   long r;
   long k;

   if (!iodigits) InitZZIO();

   b = a;

   k = sign(b);

   if (k == 0) {
      s << "0";
      return s;
   }

   if (k < 0) {
      s << "-";
      negate(b, b);
   }

   do {
      r = DivRem(b, b, ioradix);
      S.push(r);
   } while (!IsZero(b));

   r = S.pop();
   PrintDigits(s, r, 0);

   while (!S.empty()) {
      r = S.pop();
      PrintDigits(s, r, 1);
   }
      
   return s;
}

std::ostream& operator<<(std::ostream& l__s, const mat_ZZ& l__a)
{
   long nrows = l__a.NumRows();
   long ncols = l__a.NumCols();
   long i;
   l__s << "[";
   for (i = 1; i <= nrows; i++) {
     for (long j = 1; j <= ncols; j++)
       {
	 l__s << l__a(i,j) << " ";
       }
      l__s << "\n";
   } 
   l__s << "]";
   return l__s;
}
#endif

void dntl_matZZ(const mat_ZZ *A)
{
  std::cout << *A << std::endl;
}

void dntl_ZZ(const ZZ *f)
{
  std::cout << *f << std::endl;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:

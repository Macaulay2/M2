// Copyright 1995 Michael E. Stillman

#include <assert.h>
#include "bin_io.hpp"
#include "interp.hpp"

// For the format of integers, see the documentation in Macaulay2
inline unsigned char set_next(unsigned char c) { return (c + 128); }
inline unsigned char set_sign(unsigned char c) { return (c + 64); }

static int numbits(unsigned short n)
{
  int result = 0;
  if (n >= 256) { result += 8; n >>= 8; }
  if (n >= 16)  { result += 4; n >>= 4; }
  if (n >= 4)   { result += 2; n >>= 2; }
  if (n >= 2)   { result += 1; n >>= 1; }
  if (n >= 1)   { result++; }
  return result;
}

static int takebyte(mpz_t n, int bit, mpz_t temp)
{
  // first divide by 2^bit, then grab lowest limb, and and with 0xff
  mpz_div_2exp(temp, n, bit);
  unsigned int a = mpz_get_ui(temp);
  return (int) (a & 0xff);
}

static int getlength(char *&s, int &len)
{
  unsigned char c;

  if (len-- == 0) { *gError << "badly formed binary integer"; return 0; }
  c = *s++;
  if ((c & 0x80) == 0) return c;

  unsigned int ln = c & 0x7f;
  if (len-- == 0) { *gError << "badly formed binary integer"; return 0; }
  c = *s++;
  if ((c & 0x80) == 0) return (ln << 7) + c;

  ln = (ln << 7) + (c & 0x7f);
  if (len-- == 0) { *gError << "badly formed binary integer"; return 0; }
  c = *s++;
  if ((c & 0x80) == 0) return (ln << 7) + c;

  ln = (ln << 7) + (c & 0x7f);
  if (len-- == 0) { *gError << "badly formed binary integer"; return 0; }
  c = *s++;
  return (ln << 7) + (c & 0x7f);
}
static int getlength6(char *&s, int &len)
{
  unsigned char c;

  if (len-- == 0) { *gError << "badly formed binary integer"; return 0; }
  c = *s++;
  if ((c & 0x80) == 0) return c & 0x3f;

  unsigned int ln = c & 0x3f;
  if (len-- == 0) { *gError << "badly formed binary integer"; return 0; }
  c = *s++;
  if ((c & 0x80) == 0) return (ln << 7) + c;

  ln = (ln << 7) + (c & 0x7f);
  if (len-- == 0) { *gError << "badly formed binary integer"; return 0; }
  c = *s++;
  if ((c & 0x80) == 0) return (ln << 7) + c;

  ln = (ln << 7) + (c & 0x7f);
  if (len-- == 0) { *gError << "badly formed binary integer"; return 0; }
  c = *s++;
  return (ln << 7) + (c & 0x7f);
}

void bin_mpz_in(mpz_t result, char *&s, int &len)
{
  if (len <= 0) { *gError << "badly formed binary integer"; return; }
  int sgn = (0 != (0x40 & *s));
  int oldlen = len;
  int x = getlength6(s,len);
  mpz_set_si(result, x);

  int getmore2 = (len+4 == oldlen);
  int getmore3 = (0 !=  (((unsigned char) s[-1]) & 0x80));

  if (getmore2 && getmore3)
    {
      int ln = getlength(s,len);
      if (ln > len)  { *gError << "badly formed binary integer"; return; }
      while (ln > 0)
	{
	  int c = 0xff & (*s++);
	  len--;
	  ln--;
	  mpz_mul_2exp(result, result, 8);
	  mpz_add_ui(result, result, c);
	}
    }

  if (sgn) mpz_neg(result, result);
//  cout << "read integer ";
//  bignum_text_out(cout, result);
//  cout << endl;
}

void bin_mpz_out(ostream &o, mpz_t N)
{
  int nbits = mpz_sizeinbase(N, 2);  // This is suppoed to be exact, and not count negative...
  // If the number fits into the first 27 bits, use bin_int_out
  if (nbits <= 27)
    {
      int m = mpz_get_si(N);
      bin_int_out(o, m);
      return;
    }

  mpz_t temp, n;
  mpz_init(n);
  mpz_init(temp);

  int negative_mask = 0;
  int sgn = mpz_cmp_si(N, 0);
  if (sgn < 0) 
    {
      mpz_neg(n, N);
      negative_mask = 0x40;
    }
  else
    mpz_set(n, N);

  int n2 = (nbits - 20)/8;	// number of trailing bytes
  int n2b = 8 * n2;		// number of trailing bits
  int n1 = (numbits(n2) + 6) / 7; // number of bytes needed to represent n2

  char *s = new char[5 + n1 + n2];	// includes trailing null byte
  char *t = s;

  *t++ = 0x80 | takebyte(n, n2b + 21, temp) | negative_mask;
  *t++ = 0x80 | takebyte(n, n2b + 14, temp);
  *t++ = 0x80 | takebyte(n, n2b + 7, temp);
  *t++ = 0x80 | takebyte(n, n2b, temp);
  if (n2 < 0x80) 
    *t++ = n2;
  else if (n2 < 0x4000)
    {
      *t++ = 0x80 | (n2 >> 7);
      *t++ = 0x7f & n2;
    }
  else if (n2 < 0x200000)
    {
      *t++ = 0x80 | (n2 >> 14);
      *t++ = 0x80 | (n2 >> 7);
      *t++ = 0x7f & n2;
    }
  else
    {
      *t++ = 0x80 | (n2 >> 21);
      *t++ = 0x80 | (n2 >> 14);
      *t++ = 0x80 | (n2 >> 7);
      *t++ = 0x7f & n2;
    }

  while (n2 > 0)
    {
      n2--;
      *t++ = takebyte(n, 8*n2, temp);
    }
  
  for (int i=0; i<t-s; i++)
    o << s[i];

  // free up the required space!
  delete [] s;
  mpz_clear(temp);
  mpz_clear(n);
}


void bin_int_out(ostream &o, int n)
     /* append n to ds, using the format:
        b0 b1 b2 b3 ...
	where if b0 has top bit set, there is another byte,
	second bit of b0 is the sign bit (set = negative).
	last 6 bits of b0 are the top 6 bits of mantissa.
	If another byte, same with b1.
	If another byte, same with b2.
	If finally, b3 has top bit set, then the next up to
	four bytes form the length for the rest of the number.
	Finally, there are this number of 8-bit bytes of the number
	itself, to combine with the (now) 28 top bits of the mantissa */
{
  int sgn;

  if (n < 0) 
    {
      n = -n;
      sgn = 1;
    } 
  else
    sgn = 0;

  if (n < (1 << 6))
    {
      if (sgn) 
	o << (char) set_sign(n);
      else
	o << (char) n;
      return;
    }
  if (n < (1 << 13)) 
    {
      // 2 bytes
      if (sgn) 
	o << (char) set_sign(set_next(n >> 7));
      else
	o << (char) set_next(n >> 7);
      n = n % (1 << 7);
      o << (char) n;
      return;
    }
  if (n < (1 << 20)) 
    {
      // 3 bytes
      if (sgn) 
	o << (char) set_sign(set_next(n >> 14));
      else
	o << (char) set_next(n >> 14);
      n = n % (1 << 14);
      o << (char) set_next(n >> 7);
      n = n % (1 << 7);
      o << (char) n;
      return;
    }
  if (n < (1 << 27))
    {
      // 4 bytes
      if (sgn) 
	o << (char) set_sign(set_next(n >> 21));
      else
	o << (char) set_next(n >> 21);
      n = n % (1 << 21);
      o << (char) set_next(n >> 14);
      n = n % (1 << 14);
      o << (char) set_next(n >> 7);
      n = n % (1 << 7);
      o << (char) n;
      return;
    }
  // At this point: top byte contains 3 bits of mantissa,
  // next three bytes contain 7 bits each (all 4 negative), then there is a
  // length field consisting of "1", and then the bottom 8 bits
  // as a single byte.
  if (sgn) 
    o << (char) set_sign(set_next(n >> 29));
  else
    o << (char) set_next(n >> 29);
  n = n % (1 << 29);
  o << set_next(n >> 22);
  n = n % (1 << 22);
  o << (char) set_next(n >> 15);
  n = n % (1 << 15);
  o << set_next(n >> 8);
  n = n % (1 << 8);
  o << (char) 1;
  o << (char) n;
}


int bin_int_in(char *&s, int &len)
     // 
     // 
{
  if (len-- == 0) { *gError << "badly formed binary integer"; return 0; }
  unsigned char c = *s++; 
  int sign = 0x40 & c;
  int more = 0x80 & c;
  int result = 0x3f & c;
  if (more)
    {
      // get next 7 bits.
      if (len-- == 0) { *gError << "badly formed binary integer"; return 0; }
      c = *s++;
      result = result << 7;
      more = 0x80 & c;
      result += 0x7f & c;
    }
  if (more)
    {
      // get next 7 bits.
      // 3rd byte
      if (len-- == 0) { *gError << "badly formed binary integer"; return 0; }
      c = *s++;
      result = result << 7;
      more = 0x80 & c;
      result += 0x7f & c;
    }
  if (more)
    {
      // get next 7 bits.
      if (len-- == 0) { *gError << "badly formed binary integer"; return 0; }
      c = *s++;
      result = result << 7;
      more = 0x80 & c;
      result += 0x7f & c;
    }
  if (more)
    {
      // The next byte(s) form the length field
      if (len-- == 0) { *gError << "badly formed binary integer"; return 0; }
      c = *s++;
      if (c != 1) 
	{ 
	  *gError << "integer overflow: bignums not yet implemented";
	  return 0;
	}
      if (len-- == 0) { *gError << "badly formed binary integer"; return 0; }
      c = *s++;
      result = result << 8;
      result += c;
    }
  if (sign) result = -result;
  return result;
}

#if 0
void test_bin_io1(int i)
{
  ostrstream o;
  bin_int_out(o, i);
  char *s = o.str();
  int len = o.pcount();
  int j = bin_int_in(s,len);
  if (i != j)
    cout << i << "\t" << j << endl;
}
int test_bin_io()
{
  test_bin_io1((int) 0x0fefefdc);
  test_bin_io1((int) 0xffefefdc);
  test_bin_io1(- 0x000faaaa);
  for (int i=-10000; i<10000; i++)
    {
      test_bin_io1(i);
      if ((i % 10000) == 0)
	cout << "." << flush;
    }
  return 0;
}
#endif
//static int test_bin_io_int = test_bin_io();

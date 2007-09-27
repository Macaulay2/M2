
"pi" = packed integer

bits_per_fld = number of bits per subfield

"fields" of type U are packed into "bins" of unsigned type T, either uint32_t or uint64_t

The length of U is less than or equal to the length of T.

The type U is either signed or unsigned, and the templates are instantiated with
  SIGNED or SIGNED_REVERSED if U is signed
  and UNSIGNED or UNSIGNED_REVERSED if U is unsigned

Encoding from type U to an unsigned field is done in such a way that the ordering is either 
  preserved or reversed.

Example:
  n=3 bits per field, one field looks like:
 bit contents        : 000 001 010 011 100 101 110 111
 integer interpretations :
  unsigned           :  0   1   2   3   4   5   6   7     x -> 0+x    0
  signed             : -4  -3  -2  -1   0   1   2   3     x -> 4+x    4 == 1 << (n-1)
  unsigned, reversed :  7   6   5   4   3   2   1   0     x -> 7-x    7 == (1 << n) - 1
  signed, reversed   :  3   2   1   0  -1  -2  -3  -4     x -> 3-x    3 == (1 << (n-1)) - 1

As a result, arithmetic comparison of two (unsigned) bins results in a lexicographic comparison
  of the vector of fields stored within it.

An "area" will consist of a consecutive portion of an array of bins, each with the same number of
  bits per field; some fields in the last bin of the area may be unused.
Routines operating on an area will update references to pointers, so the
  next area can continue processing; thus area routines
  need not know their offset, just their length.

Justification(s) for having multiple areas (are they strong enough?) :
  1. Sometimes some of the variables are nilpotent, so any exponent needed for them is limited.
     But we have no way to take advantage of that, i.e., when a monomial multiplication results
     in a monomial with exponents so high that the corresponding ring element is 0, what do we
     do?  The element 0 does not correspond to a monomial.
  2. Some exponents are allowed to be negative, and some are not.  The division algorithm depends
     on that.  The multiplication algorithm (with overflow checking) may depend on that, i.e.,
     it may not be possible to implement it efficiently with fields of mixed type.  (CHECK!).

An "encoded monomial" will consist of a sequence of areas stored in an array of bins.

The encoding will be done with an invertible function from a vector of exponents to a vector
    of bin contents.
  The encoding function may be implemented area by area, with an internal state
    that requires initialization.
  The function will be linear, so that multiplication and
    division of monomials is easy to implement without decoding.

Monomial operations should not require decoding: comparison, multiplication, division,
  divisibility checking (how ??).  For divisibility checking, this means that the exponents
  themselves, some possibly with sign reversed, must appear among the fields, and that 
  we check just them.  A weight formed from a weight vector with components of mixed sign
  will not reflect divisibility, so must be ignored.  If the components have the same sign,
  it might be useful to check it, because it might come first and give us a quick negative answer
  half the time.

Comparison of encoded monomials is always unsigned and lexicographic; 
  thus the desired monomial ordering will dictate the encoding used.
  To implement multiple ordering steps, redundant encoding fields will be used.
     E.g., a weight can be prepended to the array of exponents.

Initial choices:
  Just one area. (?)
  All exponents appear somewhere, some reversed and some not reversed.

Thus the description of a monomial type will include:
   choice of U, the type of a field, any integer type
   choice of T, the type of a bin, either uint32_t or uint64_t
   the size of a bin in bytes
   the number of bins, numBins
   the number of fields, numFields
   the encoding initialization function
   and for each area:
     the offset of the starting bin
     the number of bins
     the number of fields
     the choice of SIGNED or SIGNED_REVERSED if U is signed
          and UNSIGNED or UNSIGNED_REVERSED if U is unsigned
     the basic comparison routine
     the multiplication routine
     the division routine; if we insist on having just one area, then we need
         a vector that tells which fields are allowed to have negative values.
     the division routine assuming divisibility, i.e., answer will have positive exponents
     the divisibility routine (reversed or normal), with an array of masks
           to tell which fields should be examined, and another array of
           masks to flip the reversed fields
   the encoding routine
   the decoding routine
   a routine for getting the multi-degree of a monomial (?)
   lcm(a,b), with lcm(a,b)/a and lcm(a,b)/b (?)
   gcd (?)



From: Michael Stillman <mike@math.cornell.edu>
Subject: Re: Re: 
Date: Thu, 1 Feb 2007 09:59:53 -0500

The following orders are the ones that we would like to be really fast:

1. grevlex -- and perhaps weighted grevlex
2. an elimination order (again perhaps weighted), or an order which  
is given first by the value of a weight vector, with ties broken by  
grevlex (or revlex).
3. lex
4. a product order, with grevlex in each block (2 blocks is the most  
important here).

Other orders are a convenience, but the ones above are the most  
heavily used.

As for different sizes in different blocks, if it is simpler and  
faster to not allow that, I would be fine with that.

Finally, I do not mind writing several "polynomial add" routines with  
different inlined calls to comparison, depending on the order.

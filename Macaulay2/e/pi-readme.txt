
"pi" = packed integer

"fields" with the same number of bits, temporarily held in integers of type U,
are packed into "bins" of unsigned type T (either uint32_t or uint64_t)

Actually, it seems that the fields could have variable widths, with no problem.

The length of U is less than or equal to the length of T.

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
  1. Some exponents are allowed to be negative, and some are not.  The division algorithm depends
     on that.  The multiplication algorithm (with overflow checking) may depend on that, i.e.,
     it may not be possible to implement it efficiently with fields of mixed type.  (CHECK!).
  2. Some variables may be exterior algebra generators, so their exponents can
     be represented with just 1 bit.

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

Monomial multiplication may need coefficients returned, too:
    1. For exterior algebra variables, the sign could be 1, 0, or -1.
       (How to compute that quickly?)
    2. For divided power variables, the product of monomials will introduce
       a product of binomial coefficients that must be returned.

Initial choices:
  1. All exponents appear somewhere, some reversed and some not reversed, so
     divisibility checking is quick.  Hmm, grevlex (i,j,k) might be encoded
     as (i+j+k,-k,-j), and -i need not be there.  Our choice means we prefer to
     encode it as (i+j+k,-k,-j,-i), trading space for time.  Divisibility
     checking needs to be fast.
  2. The routines for each area know everything about the corresponding
     variables, and that information doesn't have to be anywhere else.  For example,
     the degree is computed by summing the degrees provided by each area.
     Encoding is done by area, so a consecutive blocks of variables are
     encoded into consecutive areas.  For exterior and divided power algebras,
     the corresponding variables must be in the same area, so the area routines
     produce the coefficients, which are multiplied.  Lcm and gcd can be done
     area by area.
  3. Limit weight vectors so each is supported in a single area.  (Is this
     flexible enough?)
  4. Each area is represented by a template class.  To make it appear that
     a monomial ordering has just one area, we can have two types of area:
         -- a normal area, as described below
	 -- a composite area, which contains a sequence of others, and
  	    implements composite operations for them.  A composite area could (eventually)
  	    have weight vectors of its own that involve variables from all the
	    areas it contains, and the weights would be stored in their own
  	    bins.  A composite area would normally possess just
	    the variables that are possessed by each of its subareas, and no
	    others.  There could be (eventually) various types of composite
	    area.

Thus the description of a monomial type will include:
   TEMPLATE: (choice of U, the integer type used to hold a field)
   TEMPLATE: (choice of T, the unsigned integer type of a bin (either uint32_t or uint64_t))
   the size of a bin (T) in bytes
   the total number of variables
   the total number of fields
   the total number of bins
   and for each area:
     the offset of the starting variable
     the offset of the starting bin
     the number of variables
     the number of fields
     TEMPLATE: the number of bits per field (although future implementations of new
     	 area types might allow fields of varying widths, in which case
	 this would be an array of numbers).  This number is silently increased
         to the highest number that packs just as many fields into each bin.
     the number of fields per bin (as many as will fit, except with
     	 signed fields, we may reduce that by one when the field width
	 is a power of 2 in order to provide a guard bit at the top)
     the number of bins (as many as are needed)
     TEMPLATE: the choice of SIGNED, SIGNED_REVERSED, UNSIGNED, UNSIGNED_REVERSED;
     	 this need not agree with whether U is signed
     the encoding/decoding routines
     the comparison routine (overall ordering is lex, area by area);
     	 Optimization idea: if the user gives weights, one may be able to precompute
	 the minimum number of fields that need to be examined to give a total 
	 ordering; if grevlex is stored redundantly, e.g., as (i+j+k,-k,-j,-i),
	 the same remark applies: just check 3 fields.  If we can omit looking
   	 at enough fields, maybe we could omit looking at an entire bin, which
	 is when it would be worthwhile.   
     the multiplication routine, with overflow checking
     the multiplication routine, without overflow checking
     the division routine, with overflow checking (if the fields
         are unsigned, replace resulting negative exponents by 0 instead
	 of signalling an error)
     the division routine, without overflow checking
     the divisibility routine, with an array of masks
           to tell which fields should be examined, and another array of
           masks to flip the reversed fields
     the lcm routine, extended to provide the factors
     the gcd routine, extended to provide the factors
     noncommutative versions of the multiplication, division, lcm, and gcd routines, which
         return an integer (or two) on the side
     a flag indicating whether to use the noncommutative version -- try hard to 
         optimize the checking of this flag out (?)

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

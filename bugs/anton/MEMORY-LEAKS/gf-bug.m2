debug Core

testGF1 = (p,d,kk) -> (
   A := ambient kk;
   gen := rawMultiplicativeGenerator raw kk;
   facs := (p^d-1)//factor//toList/toList/first;
   for a in facs do assert(gen^((p^d-1)//a) != 1);
   --rawARingGFPolynomial raw kk;
   --rawARingGFCoefficients raw (kk_0^5);
   time elems := allElements(p,d-1,A); -- creating them over the finite field would be faster...
   << "fraction of unique hash vals: " << ((elems/(f -> hash raw f)//unique//length) / (#elems * 1.0)) << endl;
   time elems1 := elems/(f -> promote(f,kk));
   time elems2 := elems1/(f -> lift(f,A)); -- this one is slow for 2^13
   time elems3 := elems2/(f -> promote(f,kk));
   time assert(elems3 == elems1);
   time assert(elems2 == elems);
   time assert(# unique elems == p^d); -- this one is very slow for 2^13
   time assert(# unique elems1 == p^d);
   time m1 := promote(matrix{elems}, kk);
   time m2 := lift(m1, A);
   m1
   )

testGFpromote = (p,d,strategy) -> (
   kk := GF(p^d, Strategy=>strategy);
   testGF1(p,d,kk)
   )

end--

restart
load "gf-bug.m2"
kk = GF(7,1,Strategy=>"FlintBig")
kk = GF(7,2,Strategy=>"FlintBig")
kk = GF(7,20,Strategy=>"FlintBig")



restart
debug Core
kk = GF(7,1,Strategy=>"FlintBig")
a = new kk from rawMultiplicativeGenerator raw kk
a^3 -- crashes many times, but not all.
  -- I think I at one point found this bug: I think that flint is not doing the reduction
  -- correctly when the power is 1...  (it assumes, if I recall correctly, that 
  -- the result polynomial being reduced is of a certain size (FIND THE SPECIFIC ISSUE).
  -- work around: FlintBig should not be used at all on p^1 in any case!

restart
debug Core
kk = GF(7,2,Strategy=>"FlintBig")
a = new kk from rawMultiplicativeGenerator raw kk
a^3 -- doesn't seem to crash at all


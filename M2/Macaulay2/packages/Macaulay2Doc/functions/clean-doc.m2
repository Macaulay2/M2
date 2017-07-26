--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => {clean, (clean,RR,Matrix), (clean,RR,MutableMatrix),(clean,RR,Number), (clean,RR,RingElement)},
     Headline => "Set to zero elements that are approximately zero",
     Usage => "clean(epsilon,M)\nclean_epsilon M",
     Inputs => {
	  "epsilon" => RR,
	  "M" => Matrix => {"or a ", ofClass RingElement}
	  },
     Outputs => {
	  {ofClass Matrix, ", or ", ofClass RingElement}
	  },
     PARA{"If the input is ", ofClass Matrix, " or ", ofClass RingElement, ", then the result has the same type,
     where each real or complex number coefficient that is less than ", TT "epsilon", " in absolute value
     is replaced with zero."},
     EXAMPLE lines ///
     	  e = 1e-11;
     	  M = random(RR^4,RR^4)
	  M * (M + 1) + 1 - M^2 - M
	  clean_e oo
	  ///,
     "Cleaning a polynomial is a way to get rid of small terms.",
     EXAMPLE lines ///
     	  CC[x];
	  f = product(5,j -> x - exp(2*pi*j*ii/5))
	  clean_e f  
	  ///,
     SeeAlso => {norm, RR, CC, fillMatrix}
     }

TEST ///
R = RR_100[x,y]
F = x+.01*y^2
G = clean(.001,F^3) - (.03p200*x^2*y^2+x^3)
assert(clean(.001,G) == 0)
leadCoefficient F
norm F
norm(infinity,F)
size F

printingPrecision = 6
M = random(RR_200^10,RR_200^10)
.5 * clean(.5,M)
clean(.995,M)
norm M

R = CC[x]
M = random(R^10,R^10)
norm M
apply(flatten entries M, leadCoefficient)

-- from git issue #56
A = mutableMatrix({{1_RR}}, Dense=>true)
clean(0.1,A) -- works fine
A = mutableMatrix({{1_RR}}, Dense=>false)
assert try (clean(0.1,A);false) else true  -- not yet implemented.

///

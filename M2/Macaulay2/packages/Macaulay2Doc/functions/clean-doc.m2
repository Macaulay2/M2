--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => {clean, (clean,RR,Matrix), (clean,RR,MutableMatrix), (clean,RR,RingElement)},
     Headline => "Set to zero elements which are approximately zero",
     Usage => "clean(epsilon,M)",
     Inputs => {
	  "epsilon" => RR,
	  "M" => Matrix => {"or a ", ofClass RingElement, ", or ", ofClass MutableMatrix}
	  },
     Outputs => {
	  {ofClass Matrix, ", ", ofClass RingElement, ", or ", TO "null", ", respectively"}
	  },
     PARA{"If the input is ", ofClass Matrix, " or ", ofClass RingElement, ", then the result has the same type,
     where each real or complex number coefficient which is less than ", TT "epsilon", " in absolute value
     is replaced with zero."},
     PARA{"If the input is ", ofClass MutableMatrix, ", then ", TO "null", " is returned, and the elements of
	  the matrix are modified in place."},
     EXAMPLE lines ///
     	  M = random(RR^4,RR^5)
	  clean(.2,M)
     	  M = random(CC_100^3,CC_100^3)
	  clean(.6,M)
          norm M
	  ///,
     "Cleaning a polynomial is a way to get rid of very small terms.",
     EXAMPLE lines ///
     	  R = RR[x,y]
	  f = (1.3+1000000*y^2+.0000000001*x)^3
	  clean(.000001,f)
	  clean(2.1970000000000000000001p100,-f)  
	  norm(infinity,f)
	  norm f
	  ///,
     "Cleaning a mutable matrix is done in place.",
     EXAMPLE lines ///
     	  M = mutableMatrix(RR,4,5)
     	  fillMatrix(M,3)
	  --clean(.1,M)
	  --norm M
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
norm(1.0,F)
norm(infinity,F)
size F

printingPrecision = 6
M = random(RR_200^10,RR_200^10)
.5 * clean(.5,M)
clean(.995,M)
norm M

R = RR[x]
F = (x-1.3)*(x-2.7)*(x-1.1)
F^10
r = F % (x-1.2)
g = F // (x-1.2)
g*(x-1.2) + r == F
(-.1)*(1.2-2.7)*(.1)

R = CC[x]
M = random(R^10,R^10)
norm M
apply(flatten entries M, leadCoefficient)
///

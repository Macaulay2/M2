--		Copyright 1994-2002 by Daniel R. Grayson

use C;
use err;
use gmp;

-- types
export RawMonomial := {Monomial:void};
export RawMonomialOrNull := RawMonomial or null;
export RawArrayInt := array(int);
export RawArrayIntOrNull := RawArrayInt or null;
export RawMonomialOrdering := {MonomialOrdering:void};
export RawMonoid := {Monoid:void};
export RawMonoidOrNull := RawMonoid or null;
export RawRing := {Ring:void};
export RawRingMap := {RingMap:void};
export RawRingOrNull := RawRing or null;
export RawRingElement := {RingElement:void};
export RawRingElementOrNull := RawRingElement or null;
export RawMonomialIdeal := {MonomialIdeal:void};
export RawMonomialIdealOrNull := RawMonomialIdeal or null;
export RawFreeModule := {Module:void};
export RawFreeModuleOrNull := {Module:void} or null;
export RawMatrix := {Matrix:void};
export RawMatrixOrNull := {Matrix:void} or null;
export RawMutableMatrix := {MutableMatrix:void};
export RawMutableMatrixOrNull := {MutableMatrix:void} or null;
export IntegerPair := {a:Integer,b:Integer};
export IntegerPairOrNull := IntegerPair or null;
export IntegerOrNull := Integer or null;
export RationalOrNull := Rational or null;
export RRRorNull := RRR or null;
export RawMonomialOrderingArray := array(RawMonomialOrdering);
export RawMatrixPair := { a:RawMatrix, b:RawMatrix };
export RawMatrixPairOrNull := RawMatrixPair or null;
export RawMatrixArray := array(RawMatrix);
export RawMatrixArrayOrNull := array(RawMatrix) or null;
export RawRingElementArray := array(RawRingElement);
export RawRingElementArrayOrNull := array(RawRingElement) or null;
export RawArrayPair := { monoms:array(RawMonomial), coeffs:array(RawRingElement) };
export RawArrayPairOrNull := RawArrayPair or null;
export RawMonomialPair := { a:RawMonomial, b:RawMonomial };
export RawRingElementPair := { a:RawRingElement, b:RawRingElement };
export RawMonomialPairOrNull := RawMonomialPair or null;
export RawRingElementPairOrNull := RawRingElementPair or null;
export RawMatrixAndInt := { M:RawMatrix, i:int };
export RawComputation := {RawComputation:void};
export RawComputationOrNull := RawComputation or null;

-- functions
export EngineError(default:string):string := (
     s := Ccode(string, "(string)IM2_last_error_message()");
     if length(s) == 0 then default else s);

-- operators
export (x:RawMonomial) * (y:RawMonomial) : RawMonomialOrNull := (
     Ccode(RawMonomialOrNull, "(engine_RawMonomialOrNull)IM2_Monomial_mult((Monomial *)", x, ",(Monomial *)", y, ")" )
     );
export (x:RawMonomial) ^ (n:int) : RawMonomialOrNull := (
     Ccode(RawMonomialOrNull, "(engine_RawMonomialOrNull)IM2_Monomial_power((Monomial *)", x, ",", n, ")" )
     );
export (x:RawMonomial) / (y:RawMonomial) : RawMonomialOrNull := (
     when y^-1
     is z:RawMonomial do x*z
     is null do RawMonomialOrNull(null())
     );

export (x:RawRingElement) + (y:RawRingElement) : RawRingElementOrNull := (
     Ccode(RawRingElementOrNull,
	  "(engine_RawRingElementOrNull)IM2_RingElement_add(",
	  "(RingElement *)", x, ",(RingElement *)", y, ")" ));
export - (y:RawRingElement) : RawRingElement := (
     Ccode(RawRingElement, 
	  "(engine_RawRingElement)IM2_RingElement_negate(",
	  "(RingElement *)", y, ")" ) );
export (x:RawRingElement) - (y:RawRingElement) : RawRingElementOrNull := (
     Ccode(RawRingElementOrNull, 
	  "(engine_RawRingElementOrNull)IM2_RingElement_subtract(",
	  "(RingElement *)", x, ",(RingElement *)", y, ")" ) );
export (x:RawRingElement) * (y:RawRingElement) : RawRingElementOrNull := (
     Ccode(RawRingElementOrNull,
	  "(engine_RawRingElementOrNull)IM2_RingElement_mult(",
	  "(RingElement *)", x, ",(RingElement *)", y, ")" ));
export (x:RawRingElement) // (y:RawRingElement) : RawRingElementOrNull := (
     Ccode(RawRingElementOrNull, 
	  "(engine_RawRingElementOrNull)IM2_RingElement_div(",
	  "(RingElement *)", x, ",(RingElement *)", y, ")" ) );
export (x:RawRingElement) % (y:RawRingElement) : RawRingElementOrNull := (
     Ccode(RawRingElementOrNull, 
	  "(engine_RawRingElementOrNull)IM2_RingElement_mod(",
	  "(RingElement *)", x, ",(RingElement *)", y, ")" ) );
export (x:RawRingElement) ^ (y:Integer) : RawRingElementOrNull := ( 
     Ccode(RawRingElementOrNull, "(engine_RawRingElementOrNull)IM2_RingElement_power(", "(RingElement *)", x, ",(M2_Integer)", y, ")" ) );
export (x:RawMatrix) + (y:RawMatrix) : RawMatrixOrNull := (
     Ccode(RawMatrixOrNull, "(engine_RawMatrixOrNull)IM2_Matrix_add(", "(Matrix *)", x, ",(Matrix *)", y, ")" )
     );
export (x:RawMutableMatrix) + (y:RawMutableMatrix) : RawMutableMatrixOrNull := (
     Ccode(RawMutableMatrixOrNull, "(engine_RawMutableMatrixOrNull)IM2_MutableMatrix_add(", "(MutableMatrix *)", x, ",(MutableMatrix *)", y, ")" )
     );
-- matrix arithmetic
export - (y:RawMatrix) : RawMatrixOrNull := (
     Ccode(RawMatrixOrNull, "(engine_RawMatrixOrNull)IM2_Matrix_negate(", "(Matrix *)", y, ")" ) 
     );
export - (y:RawMutableMatrix) : RawMutableMatrix := (
     Ccode(RawMutableMatrix, "(engine_RawMutableMatrix)IM2_MutableMatrix_negate(", "(MutableMatrix *)", y, ")" ) 
     );
export (x:RawMatrix) - (y:RawMatrix) : RawMatrixOrNull := (
     Ccode(RawMatrixOrNull, "(engine_RawMatrixOrNull)IM2_Matrix_subtract(", "(Matrix *)", x, ",(Matrix *)", y, ")" ) 
     );
export (x:RawMutableMatrix) - (y:RawMutableMatrix) : RawMutableMatrixOrNull := (
     Ccode(RawMutableMatrixOrNull, "(engine_RawMutableMatrixOrNull)IM2_MutableMatrix_subtract(", "(MutableMatrix *)", x, ",(MutableMatrix *)", y, ")" ) 
     );
export (x:RawRingElement) * (y:RawMatrix) : RawMatrixOrNull := (
     Ccode(RawMatrixOrNull, "(engine_RawMatrixOrNull)IM2_Matrix_scalar_mult(", "(RingElement *)", x, ",(Matrix *)", y, ",", false, ")" )
     );
export (x:RawRingElement) * (y:RawMutableMatrix) : RawMutableMatrixOrNull := (
     Ccode(RawMutableMatrixOrNull, "(engine_RawMutableMatrixOrNull)IM2_MutableMatrix_scalar_mult(", "(RingElement *)", x, ",(MutableMatrix *)", y, ",", false, ")" )
     );
export (x:RawMatrix) * (y:RawRingElement) : RawMatrixOrNull := RawMatrixOrNull(
     Ccode(RawMatrixOrNull, "(engine_RawMatrixOrNull)IM2_Matrix_scalar_mult(", "(RingElement *)", y, ",(Matrix *)", x, ",", true, ")" )	-- opposite multiplication
     );
export (x:RawMutableMatrix) * (y:RawRingElement) : RawMutableMatrixOrNull := RawMutableMatrixOrNull(
     Ccode(RawMutableMatrixOrNull, "(engine_RawMutableMatrixOrNull)IM2_MutableMatrix_scalar_mult(", "(RingElement *)", y, ",(MutableMatrix *)", x, ",", true, ")" ) -- opposite multiplication
     );
export (x:RawMatrix) * (y:RawMatrix) : RawMatrixOrNull := (
     Ccode(RawMatrixOrNull, "(engine_RawMatrixOrNull)IM2_Matrix_mult(", "(Matrix *)", x, ",(Matrix *)", y, ",", true, ")" )
     );
export (x:RawMutableMatrix) * (y:RawMutableMatrix) : RawMutableMatrixOrNull := (
     Ccode(RawMutableMatrixOrNull, "(engine_RawMutableMatrixOrNull)IM2_MutableMatrix_mult(", "(MutableMatrix *)", x, ",(MutableMatrix *)", y, ",", true, ")" )
     );
-- 
export (x:RawMonomialIdeal) + (y:RawMonomialIdeal) : RawMonomialIdealOrNull := (
     Ccode(RawMonomialIdealOrNull, "(engine_RawMonomialIdealOrNull)", "IM2_MonomialIdeal_add((MonomialIdeal *)", x, ",(MonomialIdeal *)", y, ")" ) );
export (x:RawMonomialIdeal) - (y:RawMonomialIdeal) : RawMonomialIdealOrNull := (
     Ccode(RawMonomialIdealOrNull, "(engine_RawMonomialIdealOrNull)", "IM2_MonomialIdeal_setminus((MonomialIdeal *)", x, ",(MonomialIdeal *)", y, ")" ) );
export (x:RawMonomialIdeal) * (y:RawMonomialIdeal) : RawMonomialIdealOrNull := (
     Ccode(RawMonomialIdealOrNull, "(engine_RawMonomialIdealOrNull)", "IM2_MonomialIdeal_product((MonomialIdeal *)", x, ",(MonomialIdeal *)", y, ")" ) );

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:

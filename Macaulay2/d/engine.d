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
export RawVector := {Vector:void};
export RawVectorOrNull := {Vector:void} or null;
export RawMatrix := {Matrix:void};
export RawMatrixOrNull := {Matrix:void} or null;
export RawMutableMatrix := {MutableMatrix:void};
export RawMutableMatrixOrNull := {MutableMatrix:void} or null;
export IntegerPair := {a:Integer,b:Integer};
export IntegerPairOrNull := IntegerPair or null;
export IntegerOrNull := Integer or null;
export RawMonomialOrderingArray := array(RawMonomialOrdering);
export RawVectorArray := array(RawVector);
export RawMatrixArray := array(RawMatrix);
export RawRingElementArray := array(RawRingElement);
export RawArrayPair := { monoms:array(RawMonomial), coeffs:array(RawRingElement) };
export RawArrayPairOrNull := RawArrayPair or null;
export RawMonomialPair := { a:RawMonomial, b:RawMonomial };
export RawRingElementPair := { a:RawRingElement, b:RawRingElement };
export RawRingElementPairOrNull := RawRingElementPair or null;
export RawMatrixAndInt := { M:RawMatrix, i:int };
export LMatrixRR := { LMatrixRR:void };
export LMatrixRROrNull := LMatrixRR or null;
export LMatrixCC := { LMatrixCC:void };
export LMatrixCCOrNull := LMatrixCC or null;
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
     Ccode(RawRingElementOrNull, 
	  "(engine_RawRingElementOrNull)IM2_RingElement_power(",
	  "(RingElement *)", x, ",(M2_Integer)", y, ")" ) );
export (x:RawVector) + (y:RawVector) : RawVectorOrNull := (
     Ccode(RawVectorOrNull,
	  "(engine_RawVectorOrNull)IM2_Vector_add(",
	  "(Vector *)", x, ",(Vector *)", y,
	  ")" ));
export - (y:RawVector) : RawVector := (
     Ccode(RawVector, 
	  "(engine_RawVector)IM2_Vector_negate(",
	  "(Vector *)", y, ")" ) );
export (x:RawVector) - (y:RawVector) : RawVectorOrNull := (
     Ccode(RawVectorOrNull, 
	  "(engine_RawVectorOrNull)IM2_Vector_subtract(",
	  "(Vector *)", x, ",(Vector *)", y, ")" ) );
-- export (x:RawRingElement) * (y:RawVector) : RawVectorOrNull := (
--      Ccode(RawVectorOrNull,
-- 	  "(engine_RawVectorOrNull)IM2_Vector_scalar_mult(",
-- 	  "(RingElement *)", x, ",(Vector *)", y, ")" ));
export (x:RawVector) * (y:RawRingElement) : RawVectorOrNull := (
     Ccode(RawVectorOrNull,
	  "(engine_RawVectorOrNull)IM2_Vector_scalar_right_mult(",
	  "(Vector *)", x, ",(RingElement *)", y, ")" ));

export (x:RawMatrix) + (y:RawMatrix) : RawMatrixOrNull := (
     Ccode(RawMatrixOrNull,
	  "(engine_RawMatrixOrNull)IM2_Matrix_add(",
	  "(Matrix *)", x, ",(Matrix *)", y, ")" ));

export - (y:RawMatrix) : RawMatrix := (
     Ccode(RawMatrix, 
	  "(engine_RawMatrix)IM2_Matrix_negate(",
	  "(Matrix *)", y, ")" ) );
export (x:RawMatrix) - (y:RawMatrix) : RawMatrixOrNull := (
     Ccode(RawMatrixOrNull, 
	  "(engine_RawMatrixOrNull)IM2_Matrix_subtract(",
	  "(Matrix *)", x, ",(Matrix *)", y, ")" ) );
export (x:RawRingElement) * (y:RawMatrix) : RawMatrixOrNull := (
     Ccode(RawMatrixOrNull,
	  "(engine_RawMatrixOrNull)IM2_Matrix_scalar_mult(",
	  "(RingElement *)", x, ",(Matrix *)", y, ")" ));
-- export (x:RawMatrix) * (y:RawRingElement) : RawMatrixOrNull := (
--      Ccode(RawMatrixOrNull,
-- 	  "(engine_RawMatrixOrNull)IM2_Matrix_scalar_right_mult(",
-- 	  "(Matrix *)", x, ",(RingElement *)", y, ")" ));

export (x:RawMatrix) * (y:RawVector) : RawVectorOrNull := (
     Ccode(RawVectorOrNull,
	  "(engine_RawVectorOrNull)IM2_Matrix_scalar_mult_vec(",
	  "(Matrix *)", x, ",(Vector *)", y, ")" ));

export (x:RawMatrix) * (y:RawMatrix) : RawMatrixOrNull := (
     Ccode(RawMatrixOrNull,
	  "(engine_RawMatrixOrNull)IM2_Matrix_mult(",
	  "(Matrix *)", x, ",(Matrix *)", y, ")" ));

export (x:RawMonomialIdeal) + (y:RawMonomialIdeal) : RawMonomialIdealOrNull := (
     Ccode(RawMonomialIdealOrNull, "(engine_RawMonomialIdealOrNull)",
	  "IM2_MonomialIdeal_add((MonomialIdeal *)", x, ",(MonomialIdeal *)", y, ")" 
	  )
     );

export (x:RawMonomialIdeal) * (y:RawMonomialIdeal) : RawMonomialIdealOrNull := (
     Ccode(RawMonomialIdealOrNull, "(engine_RawMonomialIdealOrNull)",
	  "IM2_MonomialIdeal_product((MonomialIdeal *)", x, ",(MonomialIdeal *)", y, ")" 
	  )
     );


-- lapack code
export (x:LMatrixRR) + (y:LMatrixRR) : LMatrixRROrNull := (
     Ccode(LMatrixRROrNull,
	  "(engine_LMatrixRROrNull)LP_LMatrixRR_add(",
	  "(LMatrixRR *)", x, ",(LMatrixRR *)", y, ")" ));

export (x:LMatrixRR) + (y:LMatrixCC) : LMatrixCCOrNull := (
     Ccode(LMatrixCCOrNull,
	  "(engine_LMatrixCCOrNull)LP_LMatrixRC_add(",
	  "(LMatrixRR *)", x, ",(LMatrixCC *)", y, ")" ));

export (x:LMatrixCC) + (y:LMatrixRR) : LMatrixCCOrNull := (
     Ccode(LMatrixCCOrNull,
	  "(engine_LMatrixCCOrNull)LP_LMatrixCR_add(",
	  "(LMatrixCC *)", x, ",(LMatrixRR *)", y, ")" ));

export (x:LMatrixCC) + (y:LMatrixCC) : LMatrixCCOrNull := (
     Ccode(LMatrixCCOrNull,
	  "(engine_LMatrixCCOrNull)LP_LMatrixCC_add(",
	  "(LMatrixCC *)", x, ",(LMatrixCC *)", y, ")" ));

export (x:LMatrixRR) - (y:LMatrixRR) : LMatrixRROrNull := (
     Ccode(LMatrixRROrNull,
	  "(engine_LMatrixRROrNull)LP_LMatrixRR_subtract(",
	  "(LMatrixRR *)", x, ",(LMatrixRR *)", y, ")" ));

export (x:LMatrixRR) - (y:LMatrixCC) : LMatrixCCOrNull := (
     Ccode(LMatrixCCOrNull,
	  "(engine_LMatrixCCOrNull)LP_LMatrixRC_subtract(",
	  "(LMatrixRR *)", x, ",(LMatrixCC *)", y, ")" ));

export (x:LMatrixCC) - (y:LMatrixRR) : LMatrixCCOrNull := (
     Ccode(LMatrixCCOrNull,
	  "(engine_LMatrixCCOrNull)LP_LMatrixCR_subtract(",
	  "(LMatrixCC *)", x, ",(LMatrixRR *)", y, ")" ));

export (x:LMatrixCC) - (y:LMatrixCC) : LMatrixCCOrNull := (
     Ccode(LMatrixCCOrNull,
	  "(engine_LMatrixCCOrNull)LP_LMatrixCC_subtract(",
	  "(LMatrixCC *)", x, ",(LMatrixCC *)", y, ")" ));

export (x:LMatrixRR) * (y:LMatrixRR) : LMatrixRROrNull := (
     Ccode(LMatrixRROrNull,
	  "(engine_LMatrixRROrNull)LP_LMatrixRR_mult(",
	  "(LMatrixRR *)", x, ",(LMatrixRR *)", y, ")" ));

export (x:LMatrixRR) * (y:LMatrixCC) : LMatrixCCOrNull := (
     Ccode(LMatrixCCOrNull,
	  "(engine_LMatrixCCOrNull)LP_LMatrixRC_mult(",
	  "(LMatrixRR *)", x, ",(LMatrixCC *)", y, ")" ));

export (x:LMatrixCC) * (y:LMatrixRR) : LMatrixCCOrNull := (
     Ccode(LMatrixCCOrNull,
	  "(engine_LMatrixCCOrNull)LP_LMatrixCR_mult(",
	  "(LMatrixCC *)", x, ",(LMatrixRR *)", y, ")" ));

export (x:LMatrixCC) * (y:LMatrixCC) : LMatrixCCOrNull := (
     Ccode(LMatrixCCOrNull,
	  "(engine_LMatrixCCOrNull)LP_LMatrixCC_mult(",
	  "(LMatrixCC *)", x, ",(LMatrixCC *)", y, ")" ));

export - (y:LMatrixRR) : LMatrixRR := (
     Ccode(LMatrixRR, 
	  "(engine_LMatrixRR)LP_LMatrixRR_negate(",
	  "(LMatrixRR *)", y, ")" ) );

export - (y:LMatrixCC) : LMatrixCC := (
     Ccode(LMatrixCC, 
	  "(engine_LMatrixCC)LP_LMatrixCC_negate(",
	  "(LMatrixCC *)", y, ")" ) );



-- initialization
Ccode(void,"IM2_initialize()");

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d"
-- End:

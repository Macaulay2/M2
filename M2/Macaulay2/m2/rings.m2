--		Copyright 1993-1999 by Daniel R. Grayson

needs "methods.m2"

-----------------------------------------------------------------------------
-- Ring
-----------------------------------------------------------------------------

Ring.synonym = "ring"

Ring == ZZ := (R,i) -> (
     if i === 0 then 1_R == 0
     else error "comparison of ring with nonzero integer encountered"
     )

options Ring := x -> null

use Ring := x -> ( if x.?use then x.use x; x)

ZZ == Ring := (i,R) -> R == i

dim Ring := R -> (
     if R.?dim then R.dim
     else if isField R then 0
     else error("dimension of ring ", toString R, " unknown"))

char Ring := R -> (
     if R.?char then R.char 
     else error("characteristic of ", toString R, " unknown"))

baseRing = method()
baseRing Ring := R -> if R.?baseRings then last R.baseRings

errorGenCoeff = () -> error "unable to provide generators for ring over specified coefficient ring"
generators Ring := opts -> R -> (
     if opts.CoefficientRing === null or opts.CoefficientRing === R then {}
     else if opts.CoefficientRing === ZZ and R === QQ then {} -- where should we really stash this special case? (QQ is not in the class FractionField)
     else errorGenCoeff())

Ring_* := R -> generators R

numgens Ring := R -> #generators R

ring = method(TypicalValue => Ring)

-- ring Thing := x -> (
--      if x.?ring then x.ring 
--      else if instance(class x,Ring) then class x
--      else error "no ring")
-- ring Type := T -> if T.?ring then T.ring else error "no ring"

ambient Ring := Ring => R -> error "no ambient ring present"

coefficientRing = method(TypicalValue => Ring)
coefficientRing Ring := R -> error "no coefficient ring present"

isCommutative = method(TypicalValue => Boolean)
isCommutative Ring := R -> R.isCommutative

isSkewCommutative = method(TypicalValue => Boolean)
isSkewCommutative Ring := R -> false

isWeylAlgebra = method(TypicalValue => Boolean)
isWeylAlgebra Ring := R -> false

ZZ.isCommutative = true
QQ.isCommutative = true
RR.isCommutative = true
RRi.isCommutative = true

isRing = method(TypicalValue => Boolean)
isRing Thing := R -> false
isRing Ring := R -> true

isHomogeneous Ring := R -> (
     R.?isHomogeneous and R.isHomogeneous
     or
     degreeLength R == 0 
     )

promote = method(Dispatch=>{Thing,Type,Type})
lift = method(Dispatch=>{Thing,Type,Type}, Options => {Verify => true})
liftable  = method(Dispatch=>{Thing,Type,Type}, TypicalValue => Boolean)

Number   ^ Ring :=
Constant ^ Ring :=
Constant ^ RingFamily :=
RingElement ^ Ring    := (x, R) -> lift(x, R)

Number      ^ RingFamily :=
RingElement ^ RingFamily := (x, R) -> lift(x, default R)

Number      _ Ring := promote
RingElement _ Ring := promote

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:

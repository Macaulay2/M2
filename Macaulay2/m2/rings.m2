--		Copyright 1994 by Daniel R. Grayson

Ring _ String := (x,s) -> x#s		  -- gets variable from its name

random Ring := (R) -> (
     if R.?random then R.random()
     else error "no method found for item of class Ring"
     )

ZZ _ Ring := (i,R) -> (
     if i === 1 then R#1
     else if i === 0 then R#0
     else i * R#1
     )

poincare Ring := R -> poincare R^1

dim Ring := R -> (
     if R.?dim
     then R.dim
     else error("dimension of ring ", name R, " unknown"))

char Ring := R -> (
     if R.?char then R.char 
     else error("characteristic of ", name R, " unknown"))

generators Ring := R -> {}

vars Ring := R -> (
     g := generators R;
     if R.?vars then R.vars else R.vars =
     map(R^1,,{g}))

numgens Ring := R -> #generators R

ring Thing := x -> (
     if x.?ring then x.ring 
     else if instance(class x,Ring) then class x
     else error "no ring")
ring Type := T -> if T.?ring then T.ring else error "no ring"

ambient Ring := R -> error "no ambient ring present"
coefficientRing Ring := R -> error "no coefficient ring present"

isCommutative = method()
isCommutative Ring := R -> R.isCommutative

ZZ.isCommutative = true
QQ.isCommutative = true
RR.isCommutative = true

isRing = method()
isRing Thing := R -> false
isRing Ring := R -> true

isHomogeneous Ring := R -> (
     R.?isHomogeneous and R.isHomogeneous
     or
     degreeLength R == 0 
     )

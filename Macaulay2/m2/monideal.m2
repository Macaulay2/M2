-- Copyright 1995 by Michael Stillman

MonomialIdeal = new Type of MutableHashTable
monomialIdeal = method()
numgens MonomialIdeal := I -> I.numgens

generators(MonomialIdeal) := (I) -> (
     R := ring I;
     sendgg(ggPush I, ggmatrix);
     getMatrix R)

newMonomialIdeal := R -> (
     mi := new MonomialIdeal;
     sendgg(ggdup, gglength);
     mi.numgens = eePopInt();
     mi.handle = newHandle();
     mi.ring = R;
     mi)

toString MonomialIdeal := m -> if m.?name then m.name else "monomialIdeal " | toString generators m

UnaryMonomialIdealOperation := (operation) -> (m) -> (
     sendgg (ggPush m, operation);
     newMonomialIdeal ring m)

BinaryMonomialIdealOperation := (operation) -> (m,n) -> (
     sendgg (ggPush m, ggPush n, operation);
     newMonomialIdeal ring m)

net MonomialIdeal := I -> (
     if I == 0 then "0"
     else (
	  "monomialIdeal " | stack apply(
	       lines sendgg(ggPush I,ggsee,ggpop), 
	       x -> concatenate ("| ",x,"|")
	       )
	  )
     )

MonomialIdeal ^ ZZ := MonomialIdeal => (I,n) -> SimplePowerMethod(I,n)

monomialIdeal MonomialIdeal := I -> (
     if instance(I, MonomialIdeal) then (		    -- this is weird!
          sendgg(ggPush I, ggcopy);
          newMonomialIdeal ring I))

monomialIdeal Matrix := MonomialIdeal => f -> (
     sendgg(ggPush f, ggPush 0, ggmonideal);
     newMonomialIdeal ring f)

monomialIdeal(ZZ,Matrix) := MonomialIdeal => (i,m) -> (
     sendgg(ggPush m, ggPush i, ggmonideal);
     newMonomialIdeal ring m)

MonomialIdeal == MonomialIdeal := (m,n) -> (
     sendgg (ggPush m, ggPush n, ggisequal); 
     eePopBool())

MonomialIdeal == ZZ := (m,i) -> (
     if i === 0 then numgens m == 0
     else error "asked to compare monomial ideal to nonzero integer")
ZZ == MonomialIdeal := (i,m) -> m == i

MonomialIdeal +  MonomialIdeal := MonomialIdeal => BinaryMonomialIdealOperation ggadd
MonomialIdeal *  MonomialIdeal := MonomialIdeal => BinaryMonomialIdealOperation ggmult


radical MonomialIdeal := MonomialIdeal => options -> (I) -> (UnaryMonomialIdealOperation ggradical) I
--document { symbol radical,
--     TT "radical I", " -- compute the radical of a ", TO "MonomialIdeal", " I.",
--     PARA,
--     EXAMPLE "R = ZZ/101[a,b,c];",
--     EXAMPLE "I = monomialIdeal vars R",
--     EXAMPLE "J = I^3",
--     EXAMPLE "radical J"
--     }

--MonomialIdeal : Monomial := BinaryMonomialIdealOperation ggdiv
MonomialIdeal : MonomialIdeal := MonomialIdeal => BinaryMonomialIdealOperation ggdiv

saturate(MonomialIdeal, MonomialIdeal) := MonomialIdeal => options -> (I,J) -> (
     (BinaryMonomialIdealOperation ggsat) (I,J)
     )

int := BinaryMonomialIdealOperation ggintersect

intersect(List) := x -> intersect toSequence x

intersect(Sequence) := args -> (
    -- first check that all modules have the same target
    -- and the same base ring
    if #args === 0 then error "expected at least one argument";
    M := args#0;
    R := ring M;
    if class M === MonomialIdeal then (
	 if not all(args, M -> class M === MonomialIdeal and R === ring M)
	 then error "expected monomial ideals over the same ring";
	 i := 1;
	 while i < #args do (
	      M = int(M,args#i);
	      i = i+1;
	      );
	 M)
    else if class M === Module then (
    	 F := ambient args#0;
	 if not all(args, N -> ambient N == F)
	 or M.?relations 
	 and not all(args, N -> 
	      N.?relations 
	      and (N.relations == M.relations
		   or
		   image N.relations == image M.relations
		   )
	      )
    	 then error "all modules must be submodules of the same module";
    	 relns := directSum apply(args, N -> (
		   if N.?relations 
		   then generators N | N.relations
		   else generators N
		   )
	      );
    	 g := map(R^(#args),R^1, table(#args,1,x->1)) ** id_F;
	 h := modulo(g, relns);
	 if M.?relations then h = compress( h % M.relations );
    	 subquotient( h, if M.?relations then M.relations )
	 )
    else if class M === Ideal then (
	 ideal intersect apply(args,module)
	 )
    else error "expected modules, ideals, or monomial ideals"
    )

borel MonomialIdeal := MonomialIdeal => UnaryMonomialIdealOperation ggborel
isBorel MonomialIdeal := m -> (
     sendgg(ggPush m, ggisborel);
     eePopBool())

codim MonomialIdeal := m -> (
     sendgg(ggPush m, ggcodim);
     eePopInt())

poincare MonomialIdeal := M -> ( --poincare matrix m
     R := ring M;
     ZZn := degreesRing(R);
     if not M.?poincare then (
	if not M.?poincareComputation then (
	    sendgg(ggPush ZZn, ggPush M, gghilb);
	    M.poincareComputation = newHandle());
        sendgg(ggPush M.poincareComputation, ggPush (-1), ggcalc);
	sendgg(ggPush M.poincareComputation, gggetvalue);
        M.poincare = ZZn.pop());
     M.poincare)

minprimes MonomialIdeal := MonomialIdeal => m -> (
     sendgg(ggPush m, ggprimes);
     newMonomialIdeal ring m)

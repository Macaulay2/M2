--		Copyright 1993-1998 by Daniel R. Grayson and Michael E. Stillman

clone = method()

MonomialOrdering = new Type of MutableHashTable
net MonomialOrdering := toString MonomialOrdering := see

hasComponent := false					    -- not re-entrant
mo := null						    -- not re-entrant

tags := new HashTable from {
     Component => (
	  () -> (
	       if hasComponent then error "at most one 'Component' allowed in monomial ordering";
	       hasComponent = true;
	       sendgg(ggPush mo,ggMOcomponent);
	       )
	  )
     }

opts := new HashTable from {
     Lex => (
	  x -> (
	       if class x === ZZ then sendgg(ggPush x, ggPush 0, ggPush mo, ggMOlex)
	       else error "expected Lex argument to be an integer"
     	       )
	  ),
     GroupLex => (
	  x -> (
	       if class x === ZZ then sendgg(ggPush x, ggPush 1, ggPush mo, ggMOlex)
	       else error "expected GroupLex argument to be an integer"
	       )
	  ),
     RevLex => (
	  x -> (
	       if class x === ZZ then sendgg(ggPush x, ggPush 0, ggPush mo, ggMOrevlex)
	       else if class x === List and all(x,i->class i === ZZ) then sendgg(ggPush x,ggPush mo,ggMOrevlex)
	       else error "expected RevLex argument to be an integer or list of integers"
	       )
	  ),
     GroupRevLex => (
	  x -> (
	       if class x === ZZ then sendgg(ggPush x, ggPush 1, ggPush mo, ggMOrevlex)
	       else if class x === List and all(x,i->class i === ZZ) then sendgg(ggPush x,ggPush mo,ggMOrevlex)
	       else error "expected GroupRevLex argument to be an integer or list of integers"
	       )
	  ),
     NCLex => (
	  x -> (
	       if class x === ZZ then sendgg(ggPush x, ggPush mo,ggMONClex)
	       else error "expected NCLex argument to be an integer"
	       )
	  ),
     Weights => (
	  x -> (
	       if class x === List and all(x,i->class i === ZZ) then sendgg(ggPush x,ggPush mo,ggMOwtfcn)
	       else error "expected 'Weights' argument to be a list of integers"
	       )
	  )
     }

types := new HashTable from {
     Option => (
	  val -> (
	       key := val#0;
	       if opts#?key then opts#key val#1
	       else error ("unknown ordering keyword: ", toString key);
	       )
	  ),
     Eliminate => (
	  x -> sendgg(ggPush toList (x#0 : 1), ggPush mo,ggMOwtfcn)
	  )
     }

monomialOrdering = args -> (
     hasComponent = false;
     mo = new MonomialOrdering;
     mo.handle = newHandle ggMOinit;
     if class args =!= List and class args =!= Sequence then args = {args};
     scan(args, val -> (
	       if tags#?val then tags#val()
	       else if types#?(class val) then types#(class val) val
	       else error ("unknown ordering option: ", toString val)));
     if not hasComponent then sendgg(ggPush mo, ggMOcomponent);
     m := mo;
     mo = null;
     m)

clone MonomialOrdering := (mo) -> (
     o := new MonomialOrdering;
     o.handle = newHandle (ggPush mo, ggMOclone);
     o
     )

MonomialOrdering ** MonomialOrdering := (mo,mo2) -> (
     result := clone mo;
     sendgg(ggPush result,ggPush mo2,ggMOproduct);
     result)

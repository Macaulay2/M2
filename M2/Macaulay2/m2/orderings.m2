--		Copyright 1993-1998 by Daniel R. Grayson and Michael E. Stillman

MonomialOrdering = new Type of MutableHashTable
net MonomialOrdering := name MonomialOrdering := see

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
	       if class x === ZZ then sendgg(ggPush mo,ggPush x, ggMOlex)
	       else if class x === List and all(x,i->class i === ZZ) then sendgg(ggPush mo,ggPush x,ggMOlex)
	       else error "expected Lex argument to be an integer or list of integers"
     	       )
	  ),
     GroupLex => (
	  x -> (
	       if class x === ZZ then sendgg(ggPush mo,ggPush x, ggPush 1, ggMOlex)
	       else if class x === List and all(x,i->class i === ZZ) then sendgg(ggPush mo,ggPush x, ggPush 1, ggMOlex)
	       else error "expected Lex argument to be an integer or list of integers"
	       )
	  ),
     RevLex => (
	  x -> (
	       if class x === ZZ then sendgg(ggPush mo,ggPush x, ggMOrevlex)
	       else if class x === List and all(x,i->class i === ZZ) then sendgg(ggPush mo,ggPush x,ggMOrevlex)
	       else error "expected RevLex argument to be an integer or list of integers"
	       )
	  ),
     NCLex => (
	  x -> (
	       if class x === ZZ then sendgg(ggPush mo,ggPush x, ggMONClex)
	       else error "expected NCLex argument to be an integer"
	       )
	  ),
     Weights => (
	  x -> (
	       if class x === List and all(x,i->class i === ZZ) then sendgg(ggPush mo,ggPush x,ggMOwtfcn)
	       else error "expected 'Weights' argument to be a list of integers"
	       )
	  )
     }

types := new HashTable from {
     Option => (
	  val -> (
	       key := val#0;
	       if opts#?key then opts#key val#1
	       else error ("unknown ordering keyword: ", name key);
	       )
	  ),
     Eliminate => (
	  x -> sendgg(ggPush mo,ggPush toList (x#0 : 1), ggMOwtfcn)
	  )
     }

monomialOrdering = args -> (
     hasComponent = false;
     mo = new MonomialOrdering;
     mo.handle = newHandle ggMOinit;
     scan(sequence args, val -> (
	       if tags#?val then tags#val()
	       else if types#?(class val) then types#(class val) val
	       else error ("unknown ordering option: ", name val)));
     if not hasComponent then sendgg(ggPush mo, ggMOcomponent);
     m := mo;
     mo = null;
     m)

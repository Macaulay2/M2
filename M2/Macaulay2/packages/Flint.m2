-- -*- coding: utf-8 -*-
newPackage(
	"Flint",
    	Version => "0.1", 
    	Date => "September 20, 2011",
    	Authors => {
	     {Name => "Daniel R. Grayson", Email => "dan@math.uiuc.edu"}
	     },
    	HomePage => "http://www.math.uiuc.edu/~dan/",
    	Headline => "an interface to FLINT",
	AuxiliaryFiles => false
    	)

export { "factor2" }
debug Core
factor2 = method()
factor2 ZZ := n -> (
     if n === 0 then return expression n;
     r := Flint$factorint n;
     sign := r#0;
     r = drop(r,1);
     r = pack(2,r);
     if sign === -1 then r = prepend({sign,1},r);
     new Product from apply(r, pe -> new Power from pe))

beginDocumentation()
document { 
	Key => Flint,
	Headline => "an interface to FLINT"
	}

TEST ///
  assert ( factorint 60 === factor 60 )
///

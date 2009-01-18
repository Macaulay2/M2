-- -*- coding: utf-8 -*-
--- status: Draft
--- author(s): MES
--- notes: 

document { 
     Key => {gbSnapshot,
	  (gbSnapshot,Ideal),
	  (gbSnapshot,Matrix),
	  (gbSnapshot,Module)},
     Headline => "the Gröbner basis matrix as so far computed",
     Usage => "gbSnapshot M",
     Inputs => { "M" => {ofClass{Ideal,Matrix,Module}}},
     Outputs => { Matrix => "the Gröbner basis as so far computed"},
     "This routine is useful to be able to obtain
     partial results from a partially computed Gröbner basis.
     Little computation is done (although a minimalization, auto-reduction and
     sort is performed).  ",
     EXAMPLE lines ///
     	  R = ZZ/101[a..d]
	  I = intersect((ideal(a,b,c^3-d^3))^2,ideal(a^2-c^2,b^2-d^2))
	  gb(I, BasisElementLimit=>5)
	  gbSnapshot I
	  gb(I, BasisElementLimit=>10)
	  gbSnapshot I
	  gens gb I
	  ///,
     SeeAlso =>{ gb, "gbTrace", gbRemove}
     }

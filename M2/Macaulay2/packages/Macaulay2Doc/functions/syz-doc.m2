-- -*- coding: utf-8 -*-
--- status: Draft
--- author(s): MES
--- notes: 

document { 
     Key => syz,
     Headline => "the syzygy matrix"
     }
document {
     Key => (syz, GroebnerBasis),
     Headline => "retrieve the syzygy matrix",
     Usage => "syz G",
     Inputs => {
	  "G" => {"the Gröbner basis of a matrix ", TT "h"}
	  },
     Outputs => {
	  {"the matrix of syzygies among the columns of ", TT "h"}
	  },
     PARA{
     	  "Warning: the result may be zero if syzygies were not to be retained 
     	  during the calculation, or if the computation was not continued to a
     	  high enough degree."
          },
     PARA {
	  "The matrix of syzygies is returned without removing non-minimal syzygies.",
	  },
     EXAMPLE lines ///
     	  R = QQ[a..g];
	  I = ideal"ab2-c3,abc-def,ade-bfg"
	  G = gb(I, Syzygies=>true);
	  syz G
	  ///,
     "There appear to be 4 syzygies, but the last one is a combination of the first three:",
     EXAMPLE lines ///
	  syz gens I
     	  mingens image syz G
         ///,
     SeeAlso => {gb,mingens}
     }
document { 
     Key => {(syz,Matrix),
	  [syz,Algorithm],
	  [syz,BasisElementLimit],
	  [syz,DegreeLimit],
	  [syz,GBDegrees],
	  [syz,HardDegreeLimit],
	  [syz,PairLimit],
	  [syz,StopBeforeComputation],
	  [syz,Strategy],[syz,MaxReductionCount],
	  [syz,SyzygyLimit],
	  [syz,SyzygyRows]},
     Headline => "compute the syzygy matrix",
     Usage => "syz h",
     Inputs => {
	  "h" => {"a matrix"},
	  Algorithm => {"see ", TO [gb,Algorithm]},
	  BasisElementLimit => {"see ", TO [gb,BasisElementLimit]},
	  DegreeLimit => {"see ", TO [gb,DegreeLimit]},
	  GBDegrees => {"see ", TO [gb,GBDegrees]},
	  HardDegreeLimit => {"see ", TO [gb,HardDegreeLimit]},
      	  MaxReductionCount => ZZ => {
	       "the maximum number of reductions of an S-pair done before requeueing it, if the 
	       ", TT "Inhomogeneous", " algorithm is in use"
	       },
	  PairLimit => {"see ", TO [gb,PairLimit]},
	  StopBeforeComputation => {"see ", TO [gb,StopBeforeComputation]},
	  Strategy => {"see ", TO [gb,Strategy]},
	  SyzygyLimit => {"see ", TO [gb,SyzygyLimit]},
	  SyzygyRows => {"see ", TO [gb,SyzygyRows]}
	  },
     Outputs => {
	  Matrix => {"the matrix of minimal or trimmed generators for the syzygies among the columns of ", TT "h"}
	  },
     EXAMPLE lines ///
     	  R = QQ[a..g];
	  I = ideal"ab2-c3,abc-cef,ade-cfg"
     	  syz gens I     	       
	  ///,
     SeeAlso => {gb}
     }


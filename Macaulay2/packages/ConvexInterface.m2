newPackage(
	"ConvexInterface",
    	Version =>"0.25", 
    	Date =>"August 25, 2009",
    	Authors =>{{Name =>"Janko Boehm", 
		  Email =>"boehm@math.uni-sb.de", 
		  HomePage =>"http://www.math.uni-sb.de/ag/schreyer/jb/"}
                  },
    	Headline =>"Interface to Convex",
    	DebuggingMode =>true,
	AuxiliaryFiles => true,
        Configuration =>{"ConvexPath"=>""}
        )

-- For information see documentation key "ConvexInterface" below.


needsPackage "MapleInterface"

export({mFaces,mFacesAndDuals,mHomology,FinitelyGeneratedAbelianGroup,toFile,readFaces})


-- if you want to put the library convex.m in a non standard directory
-- on a Windows system, then write the path to this directory as follows:
-- "ConvexPath"=>///C:\\Programme\\Maple 9.5\\convex///

pathconvex:=((options ConvexInterface).Configuration)#"ConvexPath"

pathconvex="\""|pathconvex|"\""


mFaces=method(Options=>{toFile=>null})
mFaces(List):=opts->(L)->(
L1:=toString apply(L,entries);
---------------------------------------
mapleprogram:=
///
libname0:=libname:
libname:=libname0,placeholder2;
with(convex):
P:=convhull(op(placeholder1));
LP:=convert(P,list):
fc:=convert(faces(P),list):
fcvert:=[]:
for j from 1 to nops(fc) do
fcvert1:=[]:
for jj from 1 to nops(fc[j]) do
f:=convert(fc[j][jj],list);
fcvert1:=[op(fcvert1),f[2]]:
od:
fcvert:=[op(fcvert),fcvert1]:
od:
returnvalue:=[LP[4],LP[7],fcvert];
///;
---------------------------------------
Lfc:=callMaple(L1,pathconvex,mapleprogram,store=>opts.toFile);
{matrix cutCone Lfc#0,matrix cutCone Lfc#1,Lfc#2}
)

cutCone=method()
cutCone(List):=(L)->(
L1:={};
a:=0;
rat:=false;
for j from 0 to #L-1 do (
  a=L#j#(#(L#j)-1);
  if a!=1 then rat=true;
  L1=append(L1,apply(toList(0..#(L#j)-2),jj->L#j#jj/a));
);
if rat==true then return(apply(L1,j->apply(j,jj->sub(jj,QQ))));
apply(L1,j->apply(j,jj->sub(jj,ZZ))))

{*
L={vector {1,0,0},vector {-1,0,0},vector {0,1,0},vector {0,-1,0},vector {0,0,1},vector {0,0,-1}};
P=mFaces(L)
*}



mFacesAndDuals=method(Options=>{toFile=>null})
mFacesAndDuals(List):=opts->(L)->(
L1:=toString apply(L,entries);
---------------------------------------
mapleprogram:=
///
libname0:=libname:
libname:=libname0,placeholder2;
with(convex):
P:=convhull(op(placeholder1));
LP:=convert(P,list):
fc:=convert(faces(P),list):
fcvert:=[]:
for j from 1 to nops(fc) do
fcvert1:=[]:
for jj from 1 to nops(fc[j]) do
f:=convert(fc[j][jj],list);
fcvert1:=[op(fcvert1),[f[2],f[3]]]:
od:
fcvert:=[op(fcvert),fcvert1]:
od:
returnvalue:=[LP[4],LP[7],fcvert];
///;
---------------------------------------
Lfc:=callMaple(L1,pathconvex,mapleprogram,store=>opts.toFile);
{matrix cutCone Lfc#0,matrix cutCone Lfc#1,Lfc#2}
)

{*
L={vector {1,0,0},vector {-1,0,0},vector {0,1,0},vector {0,-1,0},vector {0,0,1},vector {0,0,-1}};
P=mFacesAndDuals(L)
*}


readFaces=method()
readFaces(String):=(fn)->(
Lfc:=readMaple(fn);
{matrix cutCone Lfc#0,matrix cutCone Lfc#1,Lfc#2}
)



mHomology=method()
mHomology(List):=(L1)->(
---------------------------------------
mapleprogram:=
///
 libname0:=libname:
 libname:=libname0,placeholder2;
 with(convex):
 topcomplex:=proc(B)
   local j,jj,L;
   L:=[]:
   for j from 2 to nops(B) do
     for jj from 1 to nops(B[j]) do
       L:=[op(L),convhull(op(B[j][jj]))]: 
     od:
   od:
   return(pcomplex(op(L)));
 end proc:
 h:=convert(homology(simplicialsubdiv(topcomplex(placeholder1)),integer),list);
 returnvalue:=[];
 for j from 1 to nops(h) do
   returnvalue:=[op(returnvalue),[rank(h[j]),torsion(h[j])]];
 od:
///;
---------------------------------------
h:=callMaple(toString L1,pathconvex,mapleprogram);
apply(h,finitelyGeneratedAbelianGroup))


-- a type to accept the result of homology:

FinitelyGeneratedAbelianGroup=new Type of HashTable

FinitelyGeneratedAbelianGroup.synonym="finitely generated abelian group"
FinitelyGeneratedAbelianGroup#{Standard,AfterPrint} = m -> (
      << endl;
      << concatenate(interpreterDepth:"o") << lineNumber << " : "
      << "finitely generated abelian group"
      << endl;)

net FinitelyGeneratedAbelianGroup := (G) -> (
NG:="";
if G.rank>1 then NG=NG|net(G.rank)|" "|net(ZZ);
if G.rank==1 then NG=NG|net(ZZ);
T:=G.torsion;
if #T>0 then (
 for j from 0 to #T-1 do (
   if NG=="" then (
     NG="ZZ/"|T#j;
   ) else (
     NG=NG|" + ZZ/"|T#j;
   );
 );
);
if NG=="" then NG=net(0);
NG)



finitelyGeneratedAbelianGroup=method()
finitelyGeneratedAbelianGroup(List):=(L)->(
  new FinitelyGeneratedAbelianGroup from {
     symbol rank => L#0,
     symbol torsion => L#1}
)

--finitelyGeneratedAbelianGroup(2,{2,56})


{*
Copyright (C) [2009] [Janko Boehm]

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, see <http://www.gnu.org/licenses/>
*}



beginDocumentation()

doc ///
  Key
    ConvexInterface
  Headline
    Interface to the Maple package Convex.
  Description
    Text
      {\bf What's new:}

        {\it August 28, 2009:}
        The interface now uses indexed vertices. This substantially improves computation time and memory usage.

        {\it August 25, 2009:}
        Added an @TO Option@ @TO toFile@ to @TO mFaces@ and @TO mFacesAndDuals@ to store the result of a computation
        in a file, and a function @TO readFaces@ to read the file.

      {\bf Overview:}
      
      The goal of this package is to provide the functions of the Maple package Convex in Macaulay 2.

      The Convex package is distributed by Matthias Franz under the GNU General Public License, see 

      http://www.math.uwo.ca/~mfranz/convex

      for more information.

      {\bf Functions:}

      So far the follwing functions are accessible via ConvexInterface:

      @TO mFaces@  -- compute the faces of a convex hull

      @TO mFacesAndDuals@ -- computing the faces and their duals

      @TO mHomology@ -- Compute the homology of a (not necessarily simplicial) complex


      Other functions that may be interesting to access:
      
      dual, hilbertbasis, simplicialsubdiv, minkowskisum, newtonpolytope

      and also

      affinehull, ambientdim, arecompatible, boundary, codim, contains, containsrelint, convhull, corank, corners, crosspolytope, cube, cyclicpolytope, delaunay, dim, directsum, distance, domain, dotprod, draw, dual, edges, emptypcomplex, emptypolyhedron, emptypolytope, facefan, faces, facets, fan, flagf, flagh, fullcone, fullpolyhedron, furthestdelaunay, fvector, genhpolynomial, genhvector, hilbertbasis, homology, hplanes, hspacenos, hspaces, hvector, image, incidencematrix, incidentfacets, incidentrays, intersection, isaffine, isbounded, iscomplete, iscontained, isempty, isface, isfulldim, islinear, ispointed, ispolytopal, isquasipolytopal, isregular, issimple, issimplex, issimplicial, issimplicial1, join, lensspace, lineality, linearhull, lines, maximal, maximum, minimal, minimum, minkowskisum, modz, newtonpolytope, normalfan, pcomplex, permutahedron, plotdata, polar, poshull, posorthant, pred, preimage, projspace, proximum, randompolytope, rank, raynos, rays, readpoly, recession, regularpart, regularsubdiv, relint, simplicialsubdiv, skeleton, stdsimplex, stellarsubdiv, succ, support, surface, torsion, transversalfan, traverse, traverse2, vertexnos, vertices, volume, voronoi, wprojspace, writepoly, zerocone, zerofan

      {\bf Setup:}

      This package needs the package @TO MapleInterface@, so set this up first.

      Install the convex package in Maple, i.e., put the file

      http://www.math.uwo.ca/~mfranz/convex/files/current/convex.m

      into the lib directory inside your Maple program directory. You can test it in Maple by typing {\it with(convex);}

      Install the {\it ConvexInterface} package in M2 by typing

      installPackage("ConvexInterface")


      {\bf Additional remarks:}

      You can put the convex.m file in any directory which shows up when you type in Maple

      libname;

      You can change this global variable by editing the Maple init file.

      You can also put the convex.m file in any directory you want, as long as you
      tell M2 about the path. In this case the installation goes as follows: Do

      installPackage("MapleInterface")

      As Maple will not find the convex package you will get some errors.

      Edit the file init-MapleInterface.m2 in the directory .Macaulay2 in your home directory
      changing the line

      "ConvexPath" =""

      to

      "ConvexPath" =StringWithPathToConvex

      where StringWithPathToConvex is a string containing the path to the
      directory containing the convex.m file.

      In Unix type systems this will be something like

      "/home/boehm/convex"
     
      In Windows systems use double backslashes to separate directories
      and the triple-slash as string delimiter. See the beginning of the source code for an example.

      Then do (if you are using M2 in Windows then first exit and then start M2 again)

      installPackage("ConvexInterface",RerunExamples=>true)

      to run the examples in the documentation. This should not give any error messages.

///


doc ///
  Key
    mFaces    
    (mFaces,List)
  Headline
    Faces of a convex hull in Maple/Convex.
  Usage
    mFaces(L)
  Inputs
    L:List
       of @TO Vector@s
  Outputs
    :List
  Description
   Text
        Returns a list of 

        - a @TO Matrix@ A with the vertices of the convex hull of L in its rows

        - a @TO Matrix@ Adual with the vertices of the dual in its rows

        - a list of lists with the faces of the convex hull sorted by increasing dimension.


        The vertices of the faces are represented by the indices of the rows of A.

        This uses the Convex functions convHull and faces.
        
   Example
    L={vector {1,0,0},vector {-1,0,0},vector {0,1,0},vector {0,-1,0},vector {0,0,1},vector {0,0,-1}}
    P=mFaces(L)
///


doc ///
  Key
    mFacesAndDuals    
    (mFacesAndDuals,List)
  Headline
    Faces and their duals of a convex hull in Maple/Convex.
  Usage
    mFacesAndDuals(L)
  Inputs
    L:List
       of @TO Vector@s
  Outputs
    :List
  Description
   Text
        Returns a list of

        - a @TO Matrix@ A with the vertices of the convex hull of L in its rows

        - a @TO Matrix@ Adual with the vertices of the dual in its rows

        - a list of lists with the faces of the convex hull sorted by increasing dimension.


        Each face is a list with two elements. The first is a list of the vertices of the face, the second a list of the vertices of the dual face.

        The vertices of the faces are represented by the indices of the rows of A.

        This uses the Convex functions convHull and faces.
        
   Example
    L={vector {1,0,0},vector {-1,0,0},vector {0,1,0},vector {0,-1,0},vector {0,0,1},vector {0,0,-1}}
    P=mFacesAndDuals(L)
///


doc ///
  Key
    mHomology    
    (mHomology,List)
  Headline
    Compute the homology.
  Usage
    mHomology(C)
  Inputs
    C:List
       of lists with the faces of a complex, sorted by dimension, each face represented by a list of vertices.
  Outputs
    :List
  Description
   Text
        Returns a list of @TO FinitelyGeneratedAbelianGroup@s with the homology with closed support of C with integer coefficients.

        C does not have to be simplicial.

        This uses the Convex functions convHull, simplicialsubdiv, pcomplex and homology.

        Remark: One should also implement the relative version from Convex.
        
   Example
     C={{{}}, {{{-1, -1, -1, -1}}, {{1, 0, 0, 0}}, {{0, 1, 0, 0}}, {{0, 0, 1,0}}, {{0, 0, 0, 1}}}, {{{-1, -1, -1, -1}, {0, 1, 0, 0}}, {{-1, -1, -1,-1}, {0, 0, 1, 0}}, {{1, 0, 0, 0}, {0, 0, 1, 0}}, {{1, 0, 0, 0}, {0, 0, 0,1}}, {{0, 1, 0, 0}, {0, 0, 0, 1}}}, {}, {}, {}};
     mHomology(C)
   Text

     RP2:

   Example
     C= {{{}}, {{{-1, -1, -1, -1, -1}}, {{1, 0, 0, 0, 0}}, {{0, 1, 0, 0, 0}}, {{0, 0, 1, 0, 0}}, {{0, 0, 0, 1, 0}}, {{0, 0, 0, 0, 1}}}, {{{-1, -1, -1, -1,-1}, {1, 0, 0, 0, 0}}, {{-1, -1, -1, -1, -1}, {0, 1, 0, 0, 0}}, {{1, 0, 0, 0, 0}, {0, 1, 0, 0, 0}}, {{-1, -1, -1, -1, -1}, {0, 0, 1, 0, 0}}, {{1, 0, 0, 0, 0}, {0, 0, 1, 0, 0}}, {{0, 1, 0, 0, 0}, {0, 0, 1, 0, 0}}, {{-1, -1, -1, -1, -1}, {0, 0, 0, 1, 0}}, {{1, 0, 0, 0, 0}, {0, 0, 0, 1, 0}}, {{0, 1, 0, 0, 0}, {0, 0, 0, 1, 0}}, {{0, 0, 1, 0, 0}, {0, 0, 0, 1, 0}}, {{-1, -1, -1, -1, -1}, {0, 0, 0, 0, 1}}, {{1, 0, 0, 0, 0}, {0, 0, 0, 0, 1}}, {{0, 1, 0, 0, 0}, {0, 0, 0, 0, 1}}, {{0, 0, 1, 0, 0}, {0, 0, 0, 0, 1}}, {{0, 0, 0, 1, 0}, {0, 0, 0, 0, 1}}}, {{{-1, -1, -1, -1, -1}, {1, 0, 0, 0, 0}, {0, 1, 0, 0, 0}}, {{1, 0, 0, 0, 0}, {0, 1, 0, 0, 0}, {0, 0, 1, 0, 0}}, {{-1, -1, -1, -1, -1}, {1, 0, 0, 0, 0}, {0, 0, 0, 1, 0}}, {{-1, -1, -1, -1, -1}, {0, 0, 1, 0, 0}, {0, 0, 0, 1, 0}}, {{0, 1, 0, 0, 0}, {0, 0, 1, 0, 0}, {0, 0, 0, 1, 0}}, {{-1, -1, -1, -1, -1}, {0, 1, 0, 0, 0}, {0, 0, 0, 0, 1}}, {{-1, -1, -1, -1, -1}, {0, 0, 1, 0, 0}, {0, 0, 0, 0, 1}}, {{1, 0, 0, 0, 0}, {0, 0, 1, 0, 0}, {0, 0, 0, 0, 1}}, {{1, 0, 0, 0, 0}, {0, 0, 0, 1, 0}, {0, 0, 0, 0, 1}}, {{0, 1, 0, 0, 0}, {0, 0, 0, 1, 0}, {0, 0, 0, 0, 1}}}, {}, {}, {}};
     mHomology(C)
///



doc ///
  Key
    FinitelyGeneratedAbelianGroup
    (net,FinitelyGeneratedAbelianGroup)
  Headline
    Class of finitely generated abelian groups.
  Description
   Text
        The class of finitely generated abelian groups (finitely generated ZZ-modules).

        This is used to store the result of @TO mHomology@ in a nice way.
        
   Example
     C= {{{}}, {{{-1, -1, -1, -1, -1}}, {{1, 0, 0, 0, 0}}, {{0, 1, 0, 0, 0}}, {{0, 0, 1, 0, 0}}, {{0, 0, 0, 1, 0}}, {{0, 0, 0, 0, 1}}}, {{{-1, -1, -1, -1,-1}, {1, 0, 0, 0, 0}}, {{-1, -1, -1, -1, -1}, {0, 1, 0, 0, 0}}, {{1, 0, 0, 0, 0}, {0, 1, 0, 0, 0}}, {{-1, -1, -1, -1, -1}, {0, 0, 1, 0, 0}}, {{1, 0, 0, 0, 0}, {0, 0, 1, 0, 0}}, {{0, 1, 0, 0, 0}, {0, 0, 1, 0, 0}}, {{-1, -1, -1, -1, -1}, {0, 0, 0, 1, 0}}, {{1, 0, 0, 0, 0}, {0, 0, 0, 1, 0}}, {{0, 1, 0, 0, 0}, {0, 0, 0, 1, 0}}, {{0, 0, 1, 0, 0}, {0, 0, 0, 1, 0}}, {{-1, -1, -1, -1, -1}, {0, 0, 0, 0, 1}}, {{1, 0, 0, 0, 0}, {0, 0, 0, 0, 1}}, {{0, 1, 0, 0, 0}, {0, 0, 0, 0, 1}}, {{0, 0, 1, 0, 0}, {0, 0, 0, 0, 1}}, {{0, 0, 0, 1, 0}, {0, 0, 0, 0, 1}}}, {{{-1, -1, -1, -1, -1}, {1, 0, 0, 0, 0}, {0, 1, 0, 0, 0}}, {{1, 0, 0, 0, 0}, {0, 1, 0, 0, 0}, {0, 0, 1, 0, 0}}, {{-1, -1, -1, -1, -1}, {1, 0, 0, 0, 0}, {0, 0, 0, 1, 0}}, {{-1, -1, -1, -1, -1}, {0, 0, 1, 0, 0}, {0, 0, 0, 1, 0}}, {{0, 1, 0, 0, 0}, {0, 0, 1, 0, 0}, {0, 0, 0, 1, 0}}, {{-1, -1, -1, -1, -1}, {0, 1, 0, 0, 0}, {0, 0, 0, 0, 1}}, {{-1, -1, -1, -1, -1}, {0, 0, 1, 0, 0}, {0, 0, 0, 0, 1}}, {{1, 0, 0, 0, 0}, {0, 0, 1, 0, 0}, {0, 0, 0, 0, 1}}, {{1, 0, 0, 0, 0}, {0, 0, 0, 1, 0}, {0, 0, 0, 0, 1}}, {{0, 1, 0, 0, 0}, {0, 0, 0, 1, 0}, {0, 0, 0, 0, 1}}}, {}, {}, {}}
     H=mHomology(C)
     H#0
     H#1
///

doc ///
  Key
    toFile
    [mFaces,toFile]
    [mFacesAndDuals,toFile]
  Headline
    Store result in a file.
  Description
   Text
    If the option toFile=>fn with a @TO String@ fn is given
    then @TO mFacesAndDuals@  stores the result in a file named fn.

    This data can be read by the command @TO readFaces@.
///


doc ///
  Key
    readFaces    
    (readFaces,String)
  Headline
    Read the result of a previous mFaces or mFacesAndDuals computation.
  Usage
    readFaces(L)
  Inputs
    fn:String
  Outputs
    :List
  Description
   Text
     Read from the file fn the result of a previous @TO mFaces@ or @TO mFacesAndDuals@ computation stored via the @TO Option@ @TO toFile@.
///


{*
installPackage("ConvexInterface",RerunExamples=>true);
*}
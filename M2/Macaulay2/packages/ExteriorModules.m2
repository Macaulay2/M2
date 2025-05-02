-- -*- coding: utf-8 -*-
newPackage(
    "ExteriorModules",
    Version => "1.0", 
    Date => "May 05, 2020",
    Authors => {{Name => "Luca Amata", Email => "lamata@unime.it", HomePage => "http://mat521.unime.it/amata"},
                {Name => "Marilena Crupi", Email => "mcrupi@unime.it", HomePage => "http://www.unime.it/it/persona/marilena-crupi"}
                },
    Headline => "monomial modules over exterior algebras",
    DebuggingMode => false,
    PackageExports=>{"ExteriorIdeals", "Complexes"},
    Keywords => {"Commutative Algebra"},
    Certification => {
         "journal name" => "The Journal of Software for Algebra and Geometry",
         "journal URI" => "https://msp.org/jsag/",
         "article title" => "ExteriorModules: a package for computing monomial modules over an exterior algebra",
         "acceptance date" => "3 June 2021",
         "published article URI" => "https://msp.org/jsag/2021/11-1/p08.xhtml",
         "published article DOI" => "10.2140/jsag.2021.11.71",
         "published code URI" => "https://msp.org/jsag/2021/11-1/jsag-v11-n1-x08-ExteriorModules.zip",
         "release at publication" => "49c26ff4b584149645fb695d8d8ba7481d0167df",        -- git commit number in hex
         "version at publication" => "1.0",
         "volume number" => "11",
         "volume URI" => "https://msp.org/jsag/2021/11-1/"
         }
)
export {"createModule", "getIdeals", "isMonomialModule", "isAlmostLexModule", "almostLexModule", "isLexModule", "lexModule", "lexModuleBySequences", "isAlmostStronglyStableModule", "almostStronglyStableModule", "isAlmostStableModule", "almostStableModule", "isStronglyStableModule", "stronglyStableModule", "isStableModule", "stableModule", "initialModule", "bassNumbers"}

------------------------------------------------------------------
-- Overloaded methods declared in "ExteriorIdeals"
-- "hilbertSequence", "isHilbertSequence", "minimalBettiNumbers"
------------------------------------------------------------------


------------------------------------------------------------------------
-- create a module from a list of ideals 
------------------------------------------------------------------------
createModule = method(TypicalValue=>Module)
createModule(List,Module) := (L,F) -> (
if #L>0 then (  
    E:=ring F;
    RM:=map(E,ring L#0);
    r:=rank F;

    if #L<=r then (
        L=trim\ideal\(x->if x=={} then {0_E} else x)\flatten\entries\gens\L;
        O:=sum apply(#L,i->trim RM L#i*F_i);
    ) else error "expected a shorter list of the rank of the free module";
) else error "expected a nonempty list of ideals";
O
)

------------------------------------------------------------------------
-- get ideals from a module
------------------------------------------------------------------------
getIdeals = method(TypicalValue=>List)
getIdeals Module := M -> (
E:=ring M;
F:=ambient M;
rk:=rank F;

if isMonomialModule M then (
    ListI:=entries compress mingens M;
    r:=#ListI;
    
    I:=join(trim\ideal\rsort\(x->if x=={} then {0_E} else x)\ListI, rk-r: ideal 0_E);
) else error "expected a monomial module";
I
)

------------------------------------------------------------------------
-- compute the Hilbert Sequence of a module
------------------------------------------------------------------------
hilbertSequence Module := M -> (
E:=ring M;
F:=ambient M;
if (options E).SkewCommutative!=flatten entries vars E / index then error "expected an exterior algebra as polynomial ring";
if not isSubset(M,F) then error "expected a submodule of a free module";
n:=#flatten entries vars E;

for k from min flatten degrees F to n+max flatten degrees F list hilbertFunction(k,F/M)
)


------------------------------------------------------------------------
-- whether a module is monomial
------------------------------------------------------------------------
isMonomialModule = method(TypicalValue=>Boolean)
isMonomialModule Module := M -> initialModule M==M


------------------------------------------------------------------------
-- whether a module is almost lex
------------------------------------------------------------------------
isAlmostLexModule = method(TypicalValue=>Boolean)
isAlmostLexModule Module := M -> (

if isMonomialModule M then (
    if not all(getIdeals M,isLexIdeal) then return false;
) else error "expected a monomial module";
true
)


------------------------------------------------------------------------
-- compute an almost lex module with the same Hilbert function of a
-- module
------------------------------------------------------------------------
almostLexModule = method(TypicalValue=>Module)
almostLexModule Module := M -> createModule(lexIdeal\getIdeals M,ambient M)

------------------------------------------------------------------------
-- whether a module is lex
------------------------------------------------------------------------
isLexModule = method(TypicalValue=>Boolean)
isLexModule Module := M -> (
E:=ring M;
EL:=newRing(E, MonomialOrder=>Lex);
F:=ambient M;
RM:=map(EL,E);
r:=rank F;
m:=ideal vars EL;
n:=#flatten entries vars EL;

if not isAlmostLexModule M then return false;
if isMonomialModule M then (    
    I:=RM\getIdeals M;

    for k from 1 to r-1 do (
        idCond:={0_EL};
        if (I#k)!= ideal(0_EL) then (
            esp:=initialDegree(I#k)+(degree(F_k))#0-(degree(F_(k-1)))#0;        
              idCond=rsort unique flatten entries compress mingens m^(esp);      
        );              
        if idCond=={} then idCond={0_EL};
        if not(isSubset(ideal idCond,I#(k-1))) then return false;
    )
) else error "expected a monomial module";
true
)

------------------------------------------------------------------------
-- whether the Kruskal-Katona theorem is satisfied.
-- That is, the input sequence is a Hilbert sequence
------------------------------------------------------------------------
isHilbertSequence(List,Module) := (hs,F) -> (
E:=ring F;
gradi:=flatten degrees F;
n:=#flatten entries vars E;
r:=rank F;
lung:=max gradi-min gradi+n+1;
maxSeqI:=for k to n list binomial(n,k);

hs=join(hs,(lung-#hs):0);

if hs!=toList(lung:0) then (
    ini:=position(hs,i->i>0);
    contrzero:=#select(gradi,i -> i==ini+min gradi);
    contruno:=(hs#ini)*(maxSeqI#1) + #select(gradi,i->i==ini+1+min gradi);
    maxSeqM:=flatten join {toList(ini:0),contrzero,contruno};

    if (hs#ini>contrzero) or (hs#(ini+1)>contruno) or (#hs>lung) then return false;

    d:=ini+min gradi+1;
    while d<(max gradi+n) do (
        val:=hs#(d-min gradi);
        i:=r-1;

        while i>=0 and val>=binomial(n,d-gradi#i) do (
            val=val-binomial(n,d-gradi#i);
            i=i-1;
        );
        seq:=apply(i+1..r-1,j->{n,d-gradi#j});
        
        if i>=0 then seq=join(seq,macaulayExpansion(val,d-gradi#i));
        seq=for x in seq list {x#0,x#1+1};
        d=d+1;        
        maxSeqM=append(maxSeqM,solveMacaulayExpansion(seq));
        if hs#(d-min gradi)>maxSeqM#(d-min gradi) then return false;
    );

);
true
)

------------------------------------------------------------------------
-- compute the lex module with the given Hilbert sequence
------------------------------------------------------------------------
lexModule = method(TypicalValue=>Module)
lexModule(List, Module) := (hs,F) -> (
E:=ring F;
gradi:=flatten degrees F;
EL:=newRing(E, MonomialOrder=>Lex);
IRM:=map(E,EL);    
m:=ideal vars EL;
n:=#flatten entries vars EL;
completeHomLex:={};
ind:=0;
lung:=max gradi-min gradi+n+1;

r:=rank F;
l:=new MutableList from toList(r:{0_EL});
ltempold:=new MutableList from toList(r:{0_EL});
Mlex:=null;
   
if isHilbertSequence(hs,F) then (
    hs=join(hs,(lung-#hs):0);
    k:=(min gradi);
    while k<=n+(max gradi) do (
        dimgr:=sum apply(r,i->binomial(n,k-gradi#i));

        ind=dimgr-hs#(k-min gradi);
        c:=0;
        while ind>0 do (
            gr:=k-gradi#c;
            completeHomLex=rsort(take(unique flatten entries compress mingens m^gr, binomial(n,gr)));

            ltemp:=take(completeHomLex,ind);
            ltemp=rsort toList(set(ltemp)-shadowSet(ltempold#c));
            l#c=join(l#c,ltemp);
            ltempold#c=join(ltempold#c,take(completeHomLex,ind));   
            ind=ind-#completeHomLex;
            c=c+1; 
        );         
        k=k+1; 
    );
    
    Mlex=createModule(IRM\trim\ideal\toList l,F);

) else error "expected a Hilbert sequence";

Mlex
)

------------------------------------------------------------------------
-- compute the lex module with the same Hilbert sequence of M
------------------------------------------------------------------------
lexModule Module := M -> lexModule(hilbertSequence M,ambient M)

------------------------------------------------------------------------
-- alternative algorithm to compute the lex module with the given Hilbert sequence 
------------------------------------------------------------------------
lexModuleBySequences = method(TypicalValue=>Module)
lexModuleBySequences(List, Module) := (hs,F) -> (
E:=ring F;
gradi:=flatten degrees F;
n:=#flatten entries vars E;
r:=rank F;
seq:=new MutableList from for k to n list binomial(n,k);
l:={};
O:=null;
lung:=(max gradi-min gradi)+n+1;

if #hs<=lung then
    hs=join(hs,(lung-#hs):0);
hsp:=hs;
        
j:=0;
while j<r do (
    offset:=gradi#(r-1-j)-min gradi;
    seq#0=min(hsp#offset,1);
    if seq#0==1 then
        seq#1=min(hsp#(offset+1),n)
    else seq#1=0;     
    k:=2;
    while k<=n do (
        seq#k=min(hsp#(offset+k),solveMacaulayExpansion macaulayExpansion(seq#(k-1),k-1,Shift=>true));
        k=k+1;      
    );
    l=join(l,{toList seq});
    hsp=hsp-join(toList(offset:0),toList seq,toList(lung-n-1-offset:0));
    j=j+1;
);
I:=for x in (reverse l) list lexIdeal(x,E);     
  
if hsp==toList(lung:0) then
    O=createModule(I,F)
else error "expected a Hilbert sequence";       

O
)

------------------------------------------------------------------------
-- whether a module is almost strongly stable
------------------------------------------------------------------------
isAlmostStronglyStableModule = method(TypicalValue=>Boolean)
isAlmostStronglyStableModule Module := M -> (
if isMonomialModule M then (
    if not all(getIdeals M,isStronglyStableIdeal) then return false;
) else error "expected a monomial module";
true
)

------------------------------------------------------------------------
-- compute the smallest almost strongly stable module that contains M
------------------------------------------------------------------------
almostStronglyStableModule = method(TypicalValue=>Module)
almostStronglyStableModule Module := M -> (
if isMonomialModule M then (
    O:=createModule(stronglyStableIdeal\getIdeals M,ambient M);
) else error "expected a monomial module";
O
)

------------------------------------------------------------------------
-- whether a module is almost stable
------------------------------------------------------------------------
isAlmostStableModule = method(TypicalValue=>Boolean)
isAlmostStableModule Module := M -> (
if isMonomialModule M then (
    if not all(getIdeals M,isStableIdeal) then return false;
) else error "expected a monomial module";
true
)

------------------------------------------------------------------------
-- compute the smallest almost stable module that contains M
------------------------------------------------------------------------
almostStableModule = method(TypicalValue=>Module)
almostStableModule Module := M -> (
if isMonomialModule M then (
    O:=createModule(stableIdeal\getIdeals M,ambient M);
) else error "expected a monomial module";
O
)

------------------------------------------------------------------------
-- whether a module is strongly stable
------------------------------------------------------------------------
isStronglyStableModule = method(TypicalValue=>Boolean)
isStronglyStableModule Module := M -> (
E:=ring M;
F:=ambient M;
m:=ideal vars E;     
r:=rank F;

if not isAlmostStronglyStableModule M then return false;
if isMonomialModule M then (    
    I:=getIdeals M;

    for k from 1 to r-1 do (
        esp:=(degree(F_k))#0-(degree(F_(k-1)))#0;        
        idCond:=rsort unique flatten entries compress mingens m^(esp);    
        if idCond=={} then idCond={0_E};          
        left:=(ideal idCond)*I#k;
        if not(isSubset(left,I#(k-1))) then return false;
    )
) else error "expected a monomial module";
   
true
)

------------------------------------------------------------------------
-- compute the smallest strongly stable module that contains M
------------------------------------------------------------------------
stronglyStableModule = method(TypicalValue=>Module)
stronglyStableModule Module := M -> (
E:=ring M;
EL:=newRing(E, MonomialOrder=>Lex);
F:=ambient M;
RM:=map(EL,E);
IRM:=map(E,EL);
m:=ideal vars EL;    
r:=rank F;
O:=null;
    
if isMonomialModule M then (    
    I:=new MutableList from RM\stronglyStableIdeal\getIdeals M;
       
    k:=r-1;
    while k>0 do (
        esp:=(degree(F_k))#0-(degree(F_(k-1)))#0;        
        idCond:=rsort unique flatten entries compress mingens m^(esp); 
        if idCond=={} then idCond={0_EL};          
        idLeft:=(ideal idCond)*I#k;
        idRight:=I#(k-1);
        genLeft:=flatten entries mingens idLeft;
        genRight:=flatten entries mingens idRight;
        if not isSubset(idLeft,idRight) then
            for m in genLeft do
                if (m % idRight)!=0_EL then
                    genRight=append(genRight,m);
        if genRight=={} then genRight={0_EL};
        I#(k-1)=stronglyStableIdeal ideal rsort unique genRight;
        k=k-1;
    );
    O=createModule(IRM\ideal\rsort\mingens\toList I,F);

) else error "expected a monomial module";
O
)

------------------------------------------------------------------------
-- whether a module is stable
------------------------------------------------------------------------
isStableModule = method(TypicalValue=>Boolean)
isStableModule Module := M -> (
E:=ring M;
F:=ambient M;
m:=ideal vars E; 
r:=rank F;

if not isAlmostStableModule M then return false;
if isMonomialModule M then (    
    I:=getIdeals M; 

    for k from 1 to r-1 do (
        esp:=(degree(F_k))#0-(degree(F_(k-1)))#0;        
        idCond:=rsort unique flatten entries compress mingens m^(esp);
        if idCond=={} then idCond={0_E};           
        left:=(ideal idCond)*I#k;
        if not(isSubset(left,I#(k-1))) then return false;
    )
) else error "expected a monomial module";
true
)

------------------------------------------------------------------------
-- compute the smallest stable module that contains M
------------------------------------------------------------------------
stableModule = method(TypicalValue=>Module)
stableModule Module := M -> (
E:=ring M;
EL:=newRing(E, MonomialOrder=>Lex);
F:=ambient M;
RM:=map(EL,E);
IRM:=map(E,EL);    
m:=ideal vars EL;  
r:=rank F;
O:=null;

if isMonomialModule M then (    
    I:=new MutableList from RM\stableIdeal\getIdeals M;

    k:=r-1;
    while k>0 do (
        esp:=(degree(F_k))#0-(degree(F_(k-1)))#0;        
        idCond:=rsort unique flatten entries compress mingens m^(esp);   
        if idCond=={} then idCond={0_EL};          
        idLeft:=(ideal idCond)*I#k;
        idRight:=I#(k-1);
        genLeft:=flatten entries mingens idLeft;
        genRight:=flatten entries mingens idRight;
        if not isSubset(idLeft,idRight) then
            for m in genLeft do
                if (m % idRight)!=0_EL then
                    genRight=append(genRight,m);
        if genRight=={} then genRight={0_EL};
        I#(k-1)=stableIdeal ideal rsort unique genRight;
        k=k-1;
    );
    O=createModule(IRM\ideal\rsort\mingens\toList I,F);      

) else error "expected a monomial module";
O
)

-------------------------------------------------------------------------------------------
-- compute the (minimal) Betti numbers of M
----------------------------------------------------------------------------------------------
minimalBettiNumbers Module := opts -> M -> betti freeResolution(image mingens M, opts)


-------------------------------------------------------------------------------------------
-- compute the initial module of M
----------------------------------------------------------------------------------------------
initialModule = method(TypicalValue=>Module)
initialModule Module := M -> image monomials leadTerm gens gb M
   
-------------------------------------------------------------------------------------------
-- compute the Bass numbers of M
----------------------------------------------------------------------------------------------
bassNumbers = method(TypicalValue=>BettiTally, Options => options minimalBettiNumbers)
bassNumbers Module := BettiTally => opts -> M -> sum(ann\getIdeals M, a -> minimalBettiNumbers(a, opts))

---------------------------------------------
---------------------------------------------
-- Private Methods by ExteriorIdeals
---------------------------------------------
---------------------------------------------

------------------------------------------------------------------------
-- compute the shadow of a monomial in an exterior algebra
------------------------------------------------------------------------
shadowMon = method(TypicalValue=>Set)
shadowMon RingElement := mon -> (
E:=ring mon;
n:=(#flatten entries vars E)-1;    

if #(flatten entries monomials mon)==1 then 
    sh:=select(for k to n list (product support(mon*E_k))_E,x->x!=1);
sh
)

------------------------------------------------------------------------
-- compute the shadow of a set of monomials in an exterior algebra
------------------------------------------------------------------------
shadowSet = method(TypicalValue=>Set)
shadowSet List := l -> set flatten apply(l,x->shadowMon x)


beginDocumentation()
-------------------------------------------------------
--DOCUMENTATION ExteriorModules
-------------------------------------------------------
 
document {
     Key => {ExteriorModules},
     Headline => "a package for working with modules over exterior algebra",
     TT "ExteriorModules", " is a package for creating and manipulating modules over exterior algebra",
     PARA{"Let ", TEX///$K$///, " be a field, ", TEX///$E$///, " the exterior algebra of a finite dimensional, ", TEX///$K$///, "-vector space, and ", TEX///$F$///, " a finitely generated graded free ", TEX///$E$///, "-module with homogeneous basis ", TEX///$g_1, \ldots, g_r$///, " such that ", TEX///$\mathrm{deg}(g_1) \le \mathrm{deg}(g_2) \le \cdots \le \mathrm{deg}(g_r)$///, ". We present a ", TT "Macaulay2", " package to manage some classes of monomial submodules of ", TEX///$F$///, ". The package is an extension of the one on monomial ideals, and contains some algorithms for computing stable, strongly stable and lexicograhic ", TEX///$E$///, "-submodules of ", TEX///$F$///, ". Such a package also includes some methods to check whether a sequence of nonnegative integers is the Hilbert function of a graded ", TEX///$E$///, "-module of the form ", TEX///$F/M$///, ", with ", TEX///$M$///, " graded submodule of ", TEX///$F$///, ". Moreover, if ", TEX///$H_{F/M}$///, " is the Hilbert function of a graded ", TEX///$E$///, "-module ", TEX///$F/M$///, ", some routines are able to compute the unique lexicograhic submodule ", TEX///$L$///, " of ", TEX///$F$///, " such that ", TEX///$H_{F/M} = H_{F/L}.$///}
     }
 
document {
     Key => {createModule,(createModule,List,Module)},
     Headline => "create a module over an exterior algebra from a list of ideals in input",
     Usage => "createModule(L,F)",
     Inputs => {"L" => {"a list of ideals"},
                "F" => {"a free module"}
          },
     Outputs => {Module => {"the submodule of ", TT "F", " which is a direct sum of submodules determined by the ideals in ", TT "L"}},
     "Let ", TEX///$\{g_1,g_2,\ldots,g_r\}$///, " be a graded basis of ", TT "F", " and let be ", TEX///$L=\{I_1,I_2,\ldots,I_r\}$///, ". This method yields the following submodule of ", TT "F", ": ", TEX///$I_1 g_1 \oplus I_2 g_2 \oplus \cdots \oplus I_r g_r$///,".",
     Caveat => {"ideals and their number have to be compatible with ambient free module"},
     PARA {"Example:"},
     EXAMPLE lines ///
        E = QQ[e_1..e_4, SkewCommutative => true]
        F=E^{0,0,0}
        I_1=ideal {e_1*e_2,e_3*e_4};
        I_2=ideal {e_1*e_2,e_2*e_3*e_4};
        I_3=ideal {e_2*e_3*e_4};
        l={I_1,I_2,I_3};
        M=createModule(l,F)
     ///,
     SeeAlso =>{getIdeals}
     }
 
document {
     Key => {getIdeals,(getIdeals,Module)},
     Headline => "get component ideals from a monomial module",
     Usage => "getIdeals M",
     Inputs => {"M" => {"a monomial submodule of the ambient module over an exterior algebra"}
          },
     Outputs => {List => {"list of ideals that determine the submodules whose direct sum is ", TT "M"}},
     "Let ", TT "M", " be a submodule of ", TT "F", " and let ", TEX///$\{g_1,g_2,\ldots,g_r\}$///, " be a graded basis of ", TT "F", ". This method returns a list ", TEX///$L=\{I_1,I_2,\ldots,I_r\}$///, " such that ", TEX///$M=I_i g_i \oplus I_2 g_2 \oplus \cdots \oplus I_r g_r$///,".",
     PARA {"Example:"},
     EXAMPLE lines ///
        E = QQ[e_1..e_4, SkewCommutative => true]
        m=matrix {{e_1*e_2,e_3*e_4,0,0,0},{0,0,e_1*e_2,e_2*e_3*e_4,0},{0,0,0,0,e_2*e_3*e_4}}
        M=image m
        getIdeals M
     ///,
     SeeAlso =>{createModule}
     }
 
document {
     Key => {(hilbertSequence,Module)},
     Headline => "compute the Hilbert sequence of a given module over an exterior algebra",
     Usage => "hilbertSequence M",
     Inputs => {"M" => {"a module over an exterior algebra ", TT "E"}
      },
     Outputs => {List => {"nonnegative integers representing the Hilbert sequence of the quotient ", TT "F/M"}},
     "Let ", TEX///$\{g_1,g_2,\ldots,g_r\}$///, " be a graded basis of ", TT "F", " with ", TEX///$deg(g_i)=f_i,\ i=1, \ldots, r.$///, " Given ", TEX///$\sum_{i=f_1}^{n+f_r}{h_i t^i}$///, " the Hilbert series of a graded ", TT "E", "-module ", TEX///$F/M$///, ", the sequence ", TEX///$(h_{f_1},\ldots,h_{n+f_r})$///, " is called the Hilbert sequence of ", TEX///$F/M.$///,
     PARA {"Example:"},
     EXAMPLE lines ///
        E = QQ[e_1..e_4, SkewCommutative => true]
        M=image matrix {{e_1*e_2,e_3*e_4,0,0,0},{0,0,e_1*e_2,e_2*e_3*e_4,0},{0,0,0,0,e_2*e_3*e_4}}
        hilbertSequence M
      ///
     }
 
document {
     Key => {isMonomialModule,(isMonomialModule,Module)},
     Headline => "whether a module is monomial",
     Usage => "isMonomialModule M",
     Inputs => {"M" => {"a monomial submodule of the ambient module over an exterior algebra"}
      },
     Outputs => {Boolean => {"whether the module ", TT "M", " is monomial"}},
     "Let ", TEX///$F$///, " a free module with homogeneous basis ", TEX///$\{g_1,g_2,\ldots,g_r\}.$///, " The elements ", TEX///$e_{\sigma}g_i$///, " with ", TEX///$e_{\sigma}$///, " a monomial of ", TEX///$E$///, " are called monomials of ", TEX///$F$///, " and ", TEX///$\mathrm{deg}(e_{\sigma} g_i) = \mathrm{deg}(e_{\sigma}) + \mathrm{deg}(g_i).$///, " A graded submodule ", TT "M", " of ", TEX///$F$///, " is a monomial submodule if ", TT "M", " is a submodule generated by monomials of ", TEX///$F$///, ", i.e., ", TEX///$M=I_i g_i \oplus I_2 g_2 \oplus \cdots \oplus I_r g_r,$///, " where ", TEX///$I_i$///, " is a monomial ideal of ", TEX///$E$///, " for each ", TEX///$i.$///,
     PARA {"Example:"},
     EXAMPLE lines ///
        E=QQ[e_1..e_3,SkewCommutative=>true]
        F=E^{0,0}
        f_1=(e_1*e_2)*F_0
        f_2=(e_1*e_3)*F_0+(e_2*e_3)*F_1
        f_3=(e_1*e_2*e_3)*F_1
        M=image map(F,E^{-degree f_1,-degree f_2,-degree f_3},matrix {f_1,f_2,f_3})
        isMonomialModule M
      ///,
     SeeAlso =>{initialModule}
     }
 
document {
     Key => {isAlmostLexModule,(isAlmostLexModule,Module)},
     Headline => "whether a monomial module over an exterior algebra is almost lex",
     Usage => "isAlmostLexModule M",
     Inputs => {"M" => {"a monomial module over an exterior algebra"}
      },
     Outputs => {Boolean => {"whether the module ", TT "M", " is almost lex"}},
     "Let ", TEX///$\{g_1,g_2,\ldots,g_r\}$///, " be a graded basis of ", TT "F.", " A monomial submodule ", TEX///$M=\oplus_{i=1}
     ^{r}{I_ig_i}$///, " of ", TT "F", " is almost lex if ", TEX///$I_i$///, " is a lex ideal of ", TT "E", " for each ", TEX///$i.$///, 
     PARA {"Example:"},
     EXAMPLE lines ///
           E = QQ[e_1..e_4, SkewCommutative => true]
           F=E^{0,0}
           I_1=ideal(e_1*e_2,e_1*e_3)
           I_2=ideal(e_1*e_2,e_1*e_3,e_2*e_3)
           M=createModule({I_1,I_2},F)
           isAlmostLexModule M
           I_3=ideal(e_1*e_2,e_1*e_3, e_1*e_4)
           isAlmostLexModule createModule({I_1,I_3},F)
     ///,
     SeeAlso =>{almostLexModule}
     }
 
document {
     Key => {almostLexModule,(almostLexModule,Module)},
     Headline => "compute an almost lex module with the same Hilbert sequence of the module in input",
     Usage => "almostLexModule M",
     Inputs => {"M" => {"a monomial module over an exterior algebra"}
      },
     Outputs => {Module => {"an almost lex submodule of the ambient module with the same Hilbert sequence of ", TT "M"}},
     "Let ", TEX///$\{g_1,g_2,\ldots,g_r\}$///, " be a graded basis of ", TT "F", " and let ", TEX///$M=\oplus_{i=1}^{r}{I_ig_i}$///, " be a monomial submodule of ", TT "F", ". The almost lex module associated to ", TT "M", " is  the monomial module ", TEX///$M^{alex}=\oplus_{i=1}^{r}{J_ig_i}$///, " with ", TEX///$J_i=\mathrm{lexIdeal}\ I_i$///, " for each ", TEX///$i$///, ", i.e., the lex ideal associated to ", TEX///$I_i$///, " for each ", TEX///$i.$///,
     PARA {"Example:"},
     EXAMPLE lines ///
           E = QQ[e_1..e_4, SkewCommutative => true]
           F=E^{0,0}
           I_1=ideal(e_1*e_2,e_1*e_3,e_2*e_3)
           I_2=ideal(e_1*e_2,e_1*e_3)
           M=createModule({I_1,I_2},F)
           almostLexModule M
      ///,
     SeeAlso =>{isAlmostLexModule, lexIdeal}
     }
 
document {
     Key => {isLexModule,(isLexModule,Module)},
     Headline => "whether a monomial module over an exterior algebra is lex",
     Usage => "isLexModule M",
     Inputs => {"M" => {"a monomial module over an exterior algebra"}
      },
     Outputs => {Boolean => {"whether the module ", TT "M", " is lex"}},
     "A monomial module ", TT "M", " is lex if for all monomials ", TEX///$u,v$///, " of ", TT "F", " of the same degree with ", TEX///$v\in M$///, " and ", TEX///$u>v$///, " (> lex order) then ", TEX///$u\in M$///,".", " An equivalent definition of a lex submodule is the following one: a monomial submodule ", TEX///$M=\oplus_{i=1}^{r}{I_ig_i}$///, " of ", TT "F", " is lex if ", TEX///$I_i$///, " is a lex ideal of ", TT "E", " for each ", TEX///$i,$///, " and ", TEX///$(e_1,\dots, e_n)^{\rho_i + f_i - f_{i-1}} \subseteq I_{i-1}$///, " for ", TEX///$i = 2, \dots, r$///, " with ", TEX///$\rho_i = \mathrm{indeg}\ I_i.$///,
     PARA {"Example:"},
     EXAMPLE lines ///
           E = QQ[e_1..e_4, SkewCommutative => true]
           F=E^{0,0}
           I_1=ideal(e_1*e_2,e_1*e_3,e_2*e_3)
           I_2=ideal(e_1*e_2,e_1*e_3)
           M=createModule({I_1,I_2},F)
           Malex=almostLexModule M
           isLexModule Malex
           L=createModule({ideal(e_1*e_2,e_1*e_3*e_4),ideal(e_1*e_2*e_3*e_4)},F)
           isLexModule L
     ///,
     SeeAlso =>{lexModule}
     }
 
document {
     Key => {(isHilbertSequence,List,Module)},
     Headline => "whether the given sequence is a Hilbert sequence",
     Usage => "isHilbertSequence(hs,F)",
     Inputs => {"hs" => {"a list of integers"},
                "F" => {"a free module over an exterior algebra"}
     },
     Outputs => {Boolean => {"whether the sequence ", TT "hs", " satisfies the generalization of the Kruskal-Katona theorem in the free module ", TT "F"}},
     "Let ", TEX///$F$///, " a free module with homogeneous basis ", TEX///$\{g_1,g_2,\ldots,g_r\},$///, " with ", TEX///$deg(g_i)=f_i,\ i=1, \ldots, r.$///, " If ", TEX///$M$///, " is a graded submodule of ", TT "F", ", and ", TEX///$H_{F/M}(t) =\sum_{i=f_1}^{f_r+n}H_{F/M}(i)t^i$///, " is the Hilbert series of ", TEX///$F/M,$///, " then the sequence ", TEX///$(H_{F/M}(f_1), H_{F/M}(f_1+1), \ldots, H_{F/M}(f_r+n))\in \mathbb{N}_0^{f_r+n-f_1+1}$///, " is called the Hilbert sequence of ", TEX///$F/M$///, " and we denote it by ", TEX///$Hs_{F/M}.$///, " The integers ", TEX///$f_1, f_1+1, \ldots, f_r+n$///, " are called the ", TEX///$Hs_{F/M}$///, "-degrees.",
     PARA {"Example:"},
     EXAMPLE lines ///
        E=QQ[e_1..e_4,SkewCommutative=>true]
        F=E^{0,0}
        isHilbertSequence({2,8,3,1,0},F)
        isHilbertSequence({2,8,3,2,0},F)
      ///,
     SeeAlso =>{lexModule}
     }
 
document {
     Key => {lexModule,(lexModule,List,Module),(lexModule,Module)},
     Headline => "compute the lex submodule with a given Hilbert sequence in a free module",
     Usage => "lexModule(hs,F) or lexModule M",
     Inputs => {"hs" => {"a list of integers"},
                "F" => {"a free module over an exterior algebra"},
                "M" => {"a monomial submodule of a free module"}
      },
     Outputs => {Module => {"the lex submodule of the ambient module with Hilbert sequence ", TT "hs", " or the lex submodule of the ambient module with the same Hilbert sequence of ", TT "M"}},
     "Let ", TEX///$F$///, " a free module with homogeneous basis ", TEX///$\{g_1,g_2,\ldots,g_r\}.$///, " If ", TEX///$M$///, " is a graded submodule of ", TT "F", ", as a consequence of a generalization of the Kruskal-Katona theorem, then there exists a unique lex submodule of ", TT "F", " with the same Hilbert function as ", TT "M", ". If ", TT "M", " is a monomial submodule of ", TT "F", ", we denote by ", TEX///$M^\mathrm{lex}$///, " the unique lex submodule of ", TT "F", " with the same Hilbert function as ", TT "M", ". ", TEX///$M^\mathrm{lex}$///, " is called the lex submodule associated to ", TT "M", ".", " This construction uses the generalization of the Kruskal-Katona theorem.", 
     PARA {"Example:"},
     EXAMPLE lines ///
        E=QQ[e_1..e_4,SkewCommutative=>true]
        F=E^{0,0}
        lexModule({2,8,3,1,0},F)
        I_1=ideal(e_1*e_2,e_1*e_3,e_2*e_3)
        I_2=ideal(e_1*e_2,e_1*e_3)
        M=createModule({I_1,I_2},F)
        Mlex=lexModule M
      ///,
     SeeAlso =>{isLexModule,isHilbertSequence,lexModuleBySequences}
     }
 
document {
     Key => {lexModuleBySequences,(lexModuleBySequences,List,Module)},
     Headline => "alternative algorithm to compute the lex submodule with a given Hilbert sequence in a free module",
     Usage => "lexModuleBySequences(hs,F)",
     Inputs => {"hs" => {"a list of integers"},
                "F" => {"a free module over an exterior algebra"}
      },
     Outputs => {Module => {"the lex submodule of the free module ", TT "F", " with Hilbert sequence ", TT "hs"}},
     "Let ", TEX///$F$///, " a free module with homogeneous basis ", TEX///$\{g_1,g_2,\ldots,g_r\}.$///, " If ", TEX///$M$///, " is a graded submodule of ", TT "F", ", as a consequence of a generalization of the Kruskal-Katona theorem, then there exists a unique lex submodule of ", TT "F", " with the same Hilbert function as ", TT "M", ". If ", TT "M", " is a monomial submodule of ", TT "F", ", we denote by ", TEX///$M^\mathrm{lex}$///, " the unique lex submodule of ", TT "F", " with the same Hilbert function as ", TT "M", ". ", TEX///$M^\mathrm{lex}$///, " is called the lex submodule associated to ", TT "M", ".", " The algorithmic construction of the lex submodule is based on the additive property of Hilbert functions and on Kruskal-Katona's theorem",
     PARA {"Example:"},
     EXAMPLE lines ///
        E=QQ[e_1..e_4,SkewCommutative=>true]
        F=E^{0,0}
        lexModuleBySequences({2,8,3,1,0},F)
      ///,
     SeeAlso =>{isLexModule,isHilbertSequence,lexModule}
     }
 
document {
     Key => {isAlmostStronglyStableModule,(isAlmostStronglyStableModule,Module)},
     Headline => "whether a monomial module over an exterior algebra is almost strongly stable",
     Usage => "isAlmostStronglyStableModule M",
     Inputs => {"M" => {"a monomial module over an exterior algebra"}
      },
     Outputs => {Boolean => {"whether the module ", TT "M", " is almost strongly stable"}},
     "Let ", TEX///$\{g_1,g_2,\ldots,g_r\}$///, " be a graded basis of ", TT "F.", " A monomial submodule ", TEX///$M=\oplus_{i=1}^{r}{I_ig_i}$///, " of ", TT "F", " is almost strongly stable if ", TEX///$I_i$///, " is a strongly stable ideal of ", TT "E", " for each ", TEX///$i.$///,
     PARA {"Example:"},
     EXAMPLE lines ///
        E = QQ[e_1..e_4, SkewCommutative => true]
        F=E^{0,0}
        I_1=ideal(e_1*e_2,e_1*e_3,e_2*e_3)
        I_2=ideal(e_1*e_2,e_1*e_3)
        M=createModule({I_1,I_2},F)
        isAlmostStronglyStableModule M
        I_3=ideal(e_1*e_2,e_1*e_4)
        isAlmostStronglyStableModule createModule({I_1,I_3},F)
     ///,
     SeeAlso =>{almostStronglyStableModule}
     }
 
document {
     Key => {almostStronglyStableModule,(almostStronglyStableModule,Module)},
     Headline => "compute the smallest almost strongly stable module containing a given monomial module",
     Usage => "almostStronglyStableModule M",
     Inputs => {"M" => {"a monomial module over an exterior algebra"}
      },
     Outputs => {Module => {"the smallest almost strongly stable submodule of the ambient module containing ", TT "M"}},
     "Let ", TEX///$\{g_1,g_2,\ldots,g_r\}$///, " be a graded basis of ", TT "F", " and let ", TEX///$M=\oplus_{i=1}^{r}{I_ig_i}$///, " a monomial submodule of ", TT "F", ". The almost strongly stable module associated to ", TT "M", " is  the monomial module ", TEX///$M_{ss}=\oplus_{i=1}^{r}{J_ig_i}$///, " with ", TEX///$J_i=\mathrm{stronglyStableIdeal}\ I_i$///, " for each ", TEX///$i$///, ", i.e., the strongly stable ideal associated to ", TEX///$I_i$///, " for each ", TEX///$i.$///,
     PARA {"Example:"},
     EXAMPLE lines ///
           E = QQ[e_1..e_4, SkewCommutative => true]
           F=E^{0,0}
           I_1=ideal(e_1*e_2,e_1*e_3,e_2*e_3)
           I_2=ideal(e_1*e_2,e_1*e_4)
           M=createModule({I_1,I_2},F)
           N=almostStronglyStableModule M
     ///,
     SeeAlso =>{isAlmostStronglyStableModule}
     }
 
document {
     Key => {isAlmostStableModule,(isAlmostStableModule,Module)},
     Headline => "whether a monomial module over an exterior algebra is almost stable",
     Usage => "isAlmostStableModule M",
     Inputs => {"M" => {"a monomial module over an exterior algebra"}
      },
     Outputs => {Boolean => {"whether the module ", TT "M", " is almost stable"}},
     "Let ", TEX///$\{g_1,g_2,\ldots,g_r\}$///, " be a graded basis of ", TT "F.", " A monomial submodule ", TEX///$M=\oplus_{i=1}^{r}{I_ig_i}$///, " of ", TT "F", " is almost stable if ", TEX///$I_i$///, " is a stable ideal of ", TT "E", " for each ", TEX///$i.$///,
     PARA {"Example:"},
     EXAMPLE lines ///
           E = QQ[e_1..e_4, SkewCommutative => true]
           F=E^{0,0}
           I_1=ideal(e_1*e_2,e_1*e_3)
           I_2=ideal(e_1*e_2,e_2*e_3)
           M=createModule({I_1,I_2},F)
           isAlmostStableModule M
           I_3=ideal(e_1*e_3,e_2*e_3)
           isAlmostStableModule createModule({I_1,I_3},F)
      ///,
     SeeAlso =>{almostStableModule}
     }
 
document {
     Key => {almostStableModule,(almostStableModule,Module)},
     Headline => "compute the smallest almost stable module containing a given monomial module",
     Usage => "almostStableModule M",
     Inputs => {"M" => {"a monomial module over an exterior algebra"}
      },
     Outputs => {Module => {"the smallest almost stable submodule of the ambient module containing ", TT "M"}},
     "Let ", TEX///$\{g_1,g_2,\ldots,g_r\}$///, " be a graded basis of ", TT "F", " and let ", TEX///$M=\oplus_{i=1}^{r}{I_ig_i}$///, " a monomial submodule of ", TT "F", ". The almost stable module associated to ", TT "M", " is  the monomial module ", TEX///$M_{s}=\oplus_{i=1}^{r}{J_ig_i}$///, " with ", TEX///$J_i=\mathrm{stableIdeal}\ I_i$///, " for each ", TEX///$i$///, ", i.e., the stable ideal associated to ", TEX///$I_i$///, " for each ", TEX///$i.$///,
     PARA {"Example:"},
     EXAMPLE lines ///
           E = QQ[e_1..e_4, SkewCommutative => true]
           F=E^{0,0}
           I_1=ideal(e_1*e_2,e_1*e_3)
           I_2=ideal(e_1*e_3,e_2*e_3)
           M=createModule({I_1,I_2},F)
           N=almostStableModule M
      ///,
     SeeAlso =>{isAlmostStableModule}
     }
 
document {
     Key => {isStronglyStableModule,(isStronglyStableModule,Module)},
     Headline => "whether a monomial module over an exterior algebra is strongly stable",
     Usage => "isStronglyStableModule M",
     Inputs => {"M" => {"a monomial module over an exterior algebra"}
      },
     Outputs => {Boolean => {"whether the module ", TT "M", " is strongly stable"}},
     "Let ", TEX///$\{g_1,g_2,\ldots,g_r\}$///, " be a graded basis of ", TT "F", " with ", TEX///$deg(g_i)=f_i,\ i=1,\ldots,r.$///," A monomial submodule ", TEX///$M=\oplus_{i=1}^{r}{I_ig_i}$///, " of ", TT "F", " is strongly stable if it is almost strongly stable and ", TEX///$(x_1,\ldots,x_n)^{(f_{i+1}-f_i)} I_{i+1}$///, " belongs to ", TEX///$I_i$///, " for ", TEX///$i=1,\ldots,r-1.$///,  " A monomial ideal ", TEX///$I$///, " of ", TEX///$E$///, " is called strongly stable if for each monomial ", TEX///$e_{\sigma} \in I$///, " and each ", TEX///$j \in \sigma$///, " one has ", TEX///$e_ie_{\sigma \setminus \{j\}} \in I$///, " for all ", TEX///$i<j.$///,
     PARA {"Example:"},
     EXAMPLE lines ///
           E = QQ[e_1..e_4, SkewCommutative => true]
           F=E^{0,0}
           I_1=ideal(e_1*e_2)
           I_2=ideal(e_1*e_2*e_3,e_1*e_2*e_4,e_1*e_3*e_4)           
           M=createModule({I_1,I_2},F)
           isAlmostStronglyStableModule M
           isStronglyStableModule M
     ///,
     SeeAlso =>{isAlmostStronglyStableModule, stronglyStableModule}
     }
 
document {
     Key => {stronglyStableModule,(stronglyStableModule,Module)},
     Headline => "compute the smallest strongly stable module containing a given monomial module",
     Usage => "stronglyStableModule M",
     Inputs => {"M" => {"a monomial module over an exterior algebra"}
      },
     Outputs => {Module => {"the smallest strongly stable submodule of the ambient module containing ", TT "M"}},
     "Let ", TEX///$F$///, " be a free module with homogeneous basis ", TEX///$\{g_1,g_2,\ldots,g_r\}$///, " and let ", TEX///$M$///, " be a monomial submodule of ", TT "F", ". This method allows the construction of the smallest strongly stable submodule of ", TT "F", " containing ", TT "M", ". It is useful, although it does not preserve invariants. In fact, the computation by hand of a strongly stable submodule implies some tedious calculations overall in the case when the elements of  the homogeneous basis of ", TT "F", " have different degrees. Furthermore, it is worth pointing out that such methods are analogous to the ", TT "Macaulay2", " function ", TT "borel", " that computes the smallest borel ideal containing a given ideal.",
     PARA {"Example:"},
     EXAMPLE lines ///
           E = QQ[e_1..e_4, SkewCommutative => true]
           F=E^{0,0}
           I_1=ideal(e_1*e_2)
           I_2=ideal(e_1*e_2*e_3,e_1*e_2*e_4,e_1*e_3*e_4)           
           M=createModule({I_1,I_2},F)
           isStronglyStableModule M
           Mss=stronglyStableModule M
           isStronglyStableModule Mss
      ///,
     SeeAlso =>{isStronglyStableModule,isAlmostStronglyStableModule}
     }
 
document {
     Key => {isStableModule,(isStableModule,Module)},
     Headline => "whether a monomial module over an exterior algebra is stable",
     Usage => "isStableModule M",
     Inputs => {"M" => {"a monomial module over an exterior algebra"}
      },
     Outputs => {Boolean => {"whether the module ", TT "M", " is stable"}},
     "Let ", TEX///$\{g_1,g_2,\ldots,g_r\}$///, " be a graded basis of ", TT "F", " with ", TEX///$deg(g_i)=f_i,\ i=1,\ldots,r.$///, " A monomial submodule ", TEX///$M=\oplus_{i=1}^{r}{I_ig_i}$///, " of ", TT "F", " is stable if it is almost stable and ", TEX///$(x_1,\ldots,x_n)^{(f_{i+1}-f_i)} I_{i+1}$///, " belongs to ", TEX///$I_i$///, " for ", TEX///$i=1,\ldots,r-1.$///, " A monomial ideal ", TEX///$I$///, " of ", TEX///$E$///, " is called stable if for each monomial ", TEX///$e_{\sigma}\in I$///, " and each ", TEX///$j < \mathrm{m}(e_{\sigma})$///, " one has ", TEX///$e_j e_{{\sigma} \setminus \{\mathrm{m}(e_{\sigma})\}} \in I.$///,
     PARA {"Example:"},
     EXAMPLE lines ///
           E = QQ[e_1..e_4, SkewCommutative => true]
           F=E^{0,0}
           I_1=ideal(e_1*e_2)
           I_2=ideal(e_1*e_2*e_3,e_1*e_2*e_4,e_1*e_3*e_4)           
           M=createModule({I_1,I_2},F)
           isAlmostStableModule M
           isStableModule M
     ///,
     SeeAlso =>{isAlmostStableModule, stableModule}
     }
 
document {
     Key => {stableModule,(stableModule,Module)},
     Headline => "compute the smallest stable module containing a given monomial module",
     Usage => "stableModule M",
     Inputs => {"M" => {"a monomial module over an exterior algebra"}
      },
     Outputs => {Module => {"the smallest stable submodule of the ambient module containing ", TT "M"}},
     "Let ", TEX///$F$///, " be a free module with homogeneous basis ", TEX///$\{g_1,g_2,\ldots,g_r\}$///, " and let ", TEX///$M$///, " be a monomial submodule of ", TT "F", ". This method allows the construction of the smallest stable submodule of ", TT "F", " containing ", TT "M", ". It is useful, although it does not preserve invariants. In fact, the computation by hand of a stable submodule implies some tedious calculations overall in the case when the elements of  the homogeneous basis of ", TT "F", " have different degrees.",
     PARA {"Example:"},
     EXAMPLE lines ///
           E = QQ[e_1..e_4, SkewCommutative => true]
           F=E^{0,0}
           I_1=ideal(e_1*e_2)
           I_2=ideal(e_1*e_2*e_3,e_1*e_2*e_4,e_1*e_3*e_4)           
           M=createModule({I_1,I_2},F)
           isStableModule M
           Ms=stableModule M
           isStableModule Ms
      ///,
     SeeAlso =>{isStableModule,isAlmostStableModule}
     }
 
document {
     Key => {(minimalBettiNumbers,Module)},
     Headline => "compute the minimal Betti numbers of a given graded module",
     Usage => "minimalBettiNumbers(M, LengthLimit => lim)",
     Inputs => {"M" => {"a graded module over an exterior algebra"},
         LengthLimit => ZZ => { "only compute enough to determine the Betti table up to and including the column labelled ", TT "lim"}
      },
     Outputs => {BettiTally => {"the Betti table of the module ", TT "M", " computed using its minimal generators"}},
     "If ", TT"M", " is a graded finitely generated module over an exterior algebra ", TT "E", ", we denote by ", TEX///$\beta_{i,j}(M)=\dim_K\mathrm{Tor}_{i}^{E}(M,K)_j$///, " the graded Betti numbers of ", TT "M", ".",
     PARA {"Example:"},
     EXAMPLE lines ///
           E=QQ[e_1..e_4,SkewCommutative=>true]
           F=E^{0,0}
           I_1=ideal(e_1*e_2,e_1*e_3,e_2*e_3)
           I_2=ideal(e_1*e_2,e_1*e_3)
           M_1=createModule({I_1,I_2},F)
           J=ideal(join(flatten entries gens I_1,{e_1*e_2*e_3}))
           M_2=createModule({J,I_2},F)
           M_1==M_2
           betti M_1==betti M_2
           minimalBettiNumbers(M_1, LengthLimit => 5) == minimalBettiNumbers(M_2, LengthLimit => 5)
      ///
     }
 
document {
     Key => {initialModule,(initialModule,Module)},
     Headline => "compute the initial module of a given module",
     Usage => "initialModule M",
     Inputs => {"M" => {"a module over an exterior algebra"}
      },
     Outputs => {Module => {"the initial module of the module ", TT "M", " with default monomial order"}},
     "Let ", TEX///$F$///, " a free module with homogeneous basis ", TEX///$\{g_1,g_2,\ldots,g_r\}.$///, " The elements ", TEX///$e_{\sigma}g_i$///, " with ", TEX///$e_{\sigma}$///, " a monomial of ", TEX///$E$///, " are called monomials of ", TEX///$F$///, " and ", TEX///$\mathrm{deg}(e_{\sigma} g_i) = \mathrm{deg}(e_{\sigma}) + \mathrm{deg}(g_i).$///, " Any element ", TEX///$f$///, " of ", ///$F$///, " is a unique linear combination of monomials with coefficients in ", TEX///$K$///, ". Let > be a monomial order on ", TEX///$E$///, ". The largest monomial of ", TEX///$f$///, " is called the initial monomial of ", TEX///$f$///, " and it is denoted by ", TEX///$\mathrm{In}(f)$///, ". If ", TT "M", " is a graded submodule of ", TEX///$F$///, " then the submodule of initial terms of ", TT "M", ", denoted by ", TEX///$\mathrm{In}(M)$///, ", is the submodule of ", TEX///$F$///, " generated by the initial terms of elements of ", TT "M", ".",
     PARA {"Example:"},
     EXAMPLE lines ///
        E=QQ[e_1..e_3,SkewCommutative=>true]
        F=E^{0,0}
        f_1=(e_1*e_2)*F_0
        f_2=(e_1*e_3)*F_0+(e_2*e_3)*F_1
        f_3=(e_1*e_2*e_3)*F_1
        M=image map(F,E^{-degree f_1,-degree f_2,-degree f_3},matrix {f_1,f_2,f_3})
        initialModule M
      ///,
     SeeAlso =>{isMonomialModule}
     }
 
document {
     Key => {bassNumbers,(bassNumbers,Module),[bassNumbers,LengthLimit]},
     Headline => "compute the Bass numbers of a given graded module",
     Usage => "bassNumbers(M, LengthLimit => lim)",
     Inputs => {"M" => {"a graded module over an exterior algebra"},
         LengthLimit => ZZ => { "only compute enough to determine the Bass table up to and including the column labelled ", TT "lim"}
      },
     Outputs => {BettiTally => {"the Bass table of the module ", TT "M", " computed using its minimal generators"}},
     "If ", TT"M", " is a graded finitely generated module over an exterior algebra ", TT "E", ", we denote by ", TEX///$\beta_{i,j}(M)=\dim_K\mathrm{Tor}_{i}^{E}(M,K)_j$///, " the graded Betti numbers of ", TT "M", " and by ", TEX///$\mu_{i,j}(M) = \dim_K \mathrm{Ext}_E^i(K, M)_j$///, " the graded Bass numbers of ", TT "M", ". Let ", TEX///$M^\ast$///, " be the right (left) ", TEX///$E$///, "-module ", TEX///$\mathrm{Hom}_E(M,E).$///, " The duality between projective and injective resolutions implies the following relation between the graded Bass numbers of a module and the graded Betti numbers of its dual: ", TEX///$\beta_{i,j}(M) = \mu_{i,n-j}(M^\ast)$///, ", for all ", TEX///$i, j.$///,
     PARA {"Example:"},
     EXAMPLE lines ///
           E=QQ[e_1..e_4,SkewCommutative=>true]
           F=E^{0,0}
           I_1=ideal(e_1*e_2,e_1*e_3,e_2*e_3)
           I_2=ideal(e_1*e_2,e_1*e_3)
           M=createModule({I_1,I_2},F)
           bassNumbers(M, LengthLimit => 5)
      ///
     }
     
------------------------------------------------------------
-- TESTS
------------------------------------------------------------

----------------------------
-- Test create module
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
F=E^{0,0}
I_1=ideal(e_1*e_2,e_1*e_3,e_2*e_3)
I_2=ideal(e_1*e_2,e_1*e_3)
M=createModule({I_1,I_2},F)
assert(M==I_1*F_0+I_2*F_1)
///

----------------------------
-- Test getIdeals
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
m=matrix {{e_1*e_2,e_3*e_4,0,0},
          {0,0,e_1*e_2,e_2*e_3*e_4}}
M=image m
I_1=ideal(e_1*e_2,e_3*e_4)
I_2=ideal(e_1*e_2,e_2*e_3*e_4)
assert(getIdeals M=={I_1,I_2})
///

----------------------------
-- Test hilbertSequence
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
F=E^{0,0}
I_1=ideal(e_1*e_2,e_1*e_3,e_2*e_3)
I_2=ideal(e_1*e_2,e_1*e_3)
M=createModule({I_1,I_2},F)
assert(hilbertSequence M=={2,8,7,1,0})
///

----------------------------
-- Test isMonomialModule
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
M=image matrix {{e_1*e_2,e_3*e_4,0,0,0},
                {0,0,e_1*e_2,e_2*e_3*e_4,0},
                {0,0,0,0,e_2*e_3*e_4}}
assert(isMonomialModule M==true)
///

----------------------------
-- Test isAlmostLexModule
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
F=E^{0,0}
I_1=ideal(e_1*e_2,e_1*e_3,e_2*e_3)
I_2=ideal(e_1*e_2,e_1*e_3)
M=createModule({I_1,I_2},F)
assert(not isAlmostLexModule M)
///

----------------------------
-- Test almostLexModule
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
F=E^{0,0}
I_1=ideal(e_1*e_2,e_1*e_3,e_2*e_3)
I_2=ideal(e_1*e_2,e_1*e_3)
M=createModule({I_1,I_2},F)
Malex=almostLexModule(M)
assert(Malex==lexIdeal(I_1)*F_0+lexIdeal(I_2)*F_1)
///

----------------------------
-- Test isLexModule
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
F=E^{0,0}
I_1=ideal(e_1*e_2,e_1*e_3,e_2*e_3)
I_2=ideal(e_1*e_2,e_1*e_3)
M=createModule({I_1,I_2},F)
Malex=almostLexModule(M)
assert(not isLexModule Malex)
///

----------------------------
-- Test lexModule
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
F=E^{0,0}
I_1=ideal(e_1*e_2,e_1*e_3,e_2*e_3)
I_2=ideal(e_1*e_2,e_1*e_3)
M=createModule({I_1,I_2},F)
Mlex=lexModule(M)
K_1=ideal({e_1*e_2,e_1*e_3,e_1*e_4,e_2*e_3,e_2*e_4})
K_2=ideal({e_1*e_2*e_3,e_1*e_2*e_4,e_1*e_3*e_4})
L=createModule({K_1,K_2},F)
assert(Mlex==L)
///

----------------------------
-- Test isHilbertSequence
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
F=E^{0,0}       
assert(isHilbertSequence({2,8,3,1,0},F))
assert(not isHilbertSequence({1,8,3,1,0},F))
assert(not isHilbertSequence({2,8,12,9,0},F))
///

----------------------------
-- Test lexModuleBySequences
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
F=E^{0,0}       
assert(lexModuleBySequences({2,8,12,7,0},F)==ideal(e_1*e_2*e_3)*F_0+ideal(e_1*e_2*e_3*e_4)*F_1)
///

----------------------------
-- Test isAlmostStronglyStableModule
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
F=E^{0,0}
I_1=ideal(e_1*e_2)
I_2=ideal(e_1*e_2*e_3,e_1*e_2*e_4,e_1*e_3*e_4)   
M=createModule({I_1,I_2},F)
assert(isAlmostStronglyStableModule M)
///

----------------------------
-- Test almostStronglyStableModule
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
F=E^{0,0}
I_1=ideal(e_1*e_2)
I_2=ideal(e_1*e_2*e_3,e_1*e_2*e_4,e_1*e_3*e_4)   
M=createModule({I_1,I_2},F)
assert(almostStronglyStableModule M==M)
///

----------------------------
-- Test isAlmostStableModule
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
F=E^{0,0}
I_1=ideal(e_1*e_2)
I_2=ideal(e_1*e_2*e_3,e_1*e_2*e_4,e_1*e_3*e_4)   
M=createModule({I_1,I_2},F)
assert(isAlmostStableModule M)
///

----------------------------
-- Test almostStableModule
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
F=E^{0,0}
I_1=ideal(e_1*e_2)
I_2=ideal(e_1*e_2*e_3,e_1*e_2*e_4,e_1*e_3*e_4)   
M=createModule({I_1,I_2},F)
assert(almostStableModule M==M)
///

----------------------------
-- Test isStronglyStableModule
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
F=E^{0,0}
I_1=ideal(e_1*e_2)
I_2=ideal(e_1*e_2*e_3,e_1*e_2*e_4,e_1*e_3*e_4)   
M=createModule({I_1,I_2},F)
assert(isAlmostStronglyStableModule M)
assert(not isStronglyStableModule M)
///

----------------------------
-- Test stronglyStableModule
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
F=E^{0,0}
I_1=ideal(e_1*e_2)
I_2=ideal(e_1*e_2*e_3,e_1*e_2*e_4,e_1*e_3*e_4)   
M=createModule({I_1,I_2},F)
Mss=stronglyStableModule M
assert(not isStronglyStableModule M)
assert(isStronglyStableModule Mss)
///

----------------------------
-- Test isStableModule
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
F=E^{0,0}
I_1=ideal(e_1*e_2)
I_2=ideal(e_1*e_2*e_3,e_1*e_2*e_4,e_1*e_3*e_4)   
M=createModule({I_1,I_2},F)
assert(isAlmostStableModule M)
assert(not isStableModule M)
///

----------------------------
-- Test stableModule
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
F=E^{0,0}
I_1=ideal(e_1*e_2)
I_2=ideal(e_1*e_2*e_3,e_1*e_2*e_4,e_1*e_3*e_4)   
M=createModule({I_1,I_2},F)
Ms=stableModule M
assert(not isStableModule M)
assert(isStableModule Ms)
///

----------------------------
-- Test minimalBettiNumbers
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
F=E^{0,0}
I_1=ideal(e_1*e_2,e_1*e_3,e_2*e_3)
I_2=ideal(e_1*e_2,e_1*e_3)
M_1=createModule({I_1,I_2},F)
J=ideal(join(flatten entries gens I_1,{e_1*e_2*e_3}))
M_2=createModule({J,I_2},F)
assert(M_1==M_2)
assert(minimalBettiNumbers(M_1, LengthLimit => 5) == minimalBettiNumbers(M_2, LengthLimit => 5))
///

----------------------------
-- Test initialModule
----------------------------
TEST ///
E=QQ[e_1..e_5,SkewCommutative=>true]
F=E^{0,0}
I_1=ideal {e_1*e_2+e_3*e_4*e_5,e_1*e_3+e_4*e_5,e_2*e_3*e_4}
I_2=ideal(e_1*e_2+e_1*e_3,e_4*e_5)
M=createModule({I_1,I_2},F)
inM=initialModule M
assert(isMonomialModule inM)
assert(hilbertSequence M==hilbertSequence inM)
///

----------------------------
-- Test bassNumbers
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
F=E^{0,0}
I_1=ideal(e_1*e_2,e_1*e_3,e_2*e_3)
I_2=ideal(e_1*e_2,e_1*e_3)
M=createModule({I_1,I_2},F)
t=bassNumbers(M, LengthLimit => 5)
assert(t#(1,{2},2)==4)
assert(t#(4,{5},5)==34)
assert(t#(5,{6},6)==50)
///

end

restart
installPackage ("ExteriorIdeals", UserMode=>true, RerunExamples=>true)
installPackage ("ExteriorModules", UserMode=>true, RerunExamples=>true)
loadPackage "ExteriorModules"
viewHelp


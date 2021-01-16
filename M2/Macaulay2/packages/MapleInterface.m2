newPackage(
	"MapleInterface",
    	Version => "0.3", 
    	Date => "June 14, 2010",
    	Authors => {{Name => "Janko Boehm", 
		  Email => "boehm@mathematik.uni-kl.de", 
		  HomePage => "http://www.math.uni-sb.de/ag/schreyer/jb/"}
                  },
    	Headline => "interface to Maple",
	Keywords => {"Interfaces"},
    	DebuggingMode => false,
        Configuration => {"MapleCommand"=>"maple"},
	CacheExampleOutput => true,
	AuxiliaryFiles => true
        )

-- For information see documentation key "MapleInterface" below.

export {"callMaple","store","readMaple","msqrt","integralBasis"}

getFilename = () -> (
     filename := temporaryFileName();
     while fileExists(filename) or fileExists(filename|".txt") do filename = temporaryFileName();
     filename)

-- the Maple command
maplecommand:=((options MapleInterface).Configuration)#"MapleCommand"

callMaple=method(Options=>{store=>null})

callMaple(String):=opts->(mapleprogram)->(
callMaple("","",mapleprogram,opts))

callMaple(String,String,String):=opts->(inputdata1,inputdata2,mapleprogram)->(
L1:=changeBrackets inputdata1;
filename:= getFilename();
filename2:= getFilename();
filename3:= getFilename();
----------------------------------
mapleprogram=mapleprogram|///
fd := open("placeholder3.txt",WRITE):
fprintf(fd,convert(returnvalue,string)):
close(fd):
quit;
///;
----------------------------------
mapleprogram=replace("placeholder2",inputdata2,mapleprogram);
mapleprogram=replace("placeholder1",L1,mapleprogram);
mapleprogram=replace("placeholder3",rootPath|filename3,mapleprogram);
--print mapleprogram;
filename|".txt" << mapleprogram << endl << close;
outfile := rootPath|filename2|".txt";
command := "\""|maplecommand|"\""|" <"|rootPath|filename|".txt >"|outfile;
if debugLevel > 0 then (
     stderr << "running " << command << endl
     	    << "with input:" << endl << mapleprogram << endl);
run command;
if debugLevel > 0 then stderr << "and with output:" << endl << get outfile << endl;
if fileExists(rootPath|filename3|".txt")==false then error("Maple returned errors, see file "|rootPath|filename2|".txt for the Maple-output and "|rootPath|filename|".txt for the Maple-input");
Lfc:=changeBrackets2(get(rootPath|filename3|".txt"));
run("rm"|" "|rootPath|filename|".txt");
run("rm"|" "|rootPath|filename2|".txt");
if class(opts.store)===String then (
  run("cp"|" "|rootPath|filename3|".txt"|" "|(opts.store))
);
run("rm"|" "|rootPath|filename3|".txt");
value Lfc)


changeBrackets=method()
changeBrackets(String):=(S)->(
replace("[}]","]",replace("[{]","[",S)))

changeBrackets2=method()
changeBrackets2(String):=(S)->(
replace("[]]","}",replace("[[]","{",S)))

--changeBrackets("{{1,2},{3,4}}")

readMaple=method()
readMaple String := fn-> value changeBrackets2 get fn

msqrt=method()
msqrt(ZZ,ZZ):=(a,b)->(
inputdata1:=toString {a,b};
inputdata2:="msqrt";
mapleprogram:="with(numtheory,placeholder2):returnvalue:=placeholder2(op(placeholder1));";
L:=callMaple(inputdata1,inputdata2,mapleprogram);
L)


integralBasis=method()
integralBasis(Ideal):=(I)->(
v:=gens ring I;
if rank source gens I>1 then error("expected a hypersurface");
f:=I_0;
if #v>2 then error("expected an ideal in a ring with 2 variables");
inputdata1:=(toString(f))|","|toString(v#1)|","|toString(v#0),
inputdata2:="integral_basis";
mapleprogram:="with(algcurves,placeholder2):returnvalue:=placeholder2(placeholder1);";
L:=callMaple(inputdata1,inputdata2,mapleprogram);
matrix {L})

----------------------------------------------------------------------

-*
Copyright (C) [2009] [Janko Boehm]

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, see <http://www.gnu.org/licenses/>
*-


beginDocumentation()

doc ///
  Key
    MapleInterface
  Headline
    Interface to Maple.
  Description
    Text
      {\bf What's new:}

        {\it December 1, 2009:}
        Added functions @TO msqrt@ computing the modular square root
        (for large numbers) and @TO integralBasis@ computing an integral basis of 
        an algebraic function field.

        {\it October 4, 2009:}
        Added method @TO (callMaple,String)@.

        {\it August 25, 2009:}
        Added an @TO Option@ @TO store@ to @TO callMaple@ to store the result of a computation
        in a file, and a function @TO readMaple@ to read the file.


      {\bf Overview:}
      
      The goal of this package is to provide an interface to run Maple scripts from Macaulay 2.

      {\bf Setup:}

      This package is just an interface. To use it you need a licence for Maple and have the program installed on your machine.

      Install this package by doing

      installPackage("MapleInterface")

      Edit the file init-MapleInterface.m2 in the directory .Macaulay2 in your home directory
      changing the line

      "MapleCommand" => "maple"

      to

      "MapleCommand" => StringWithMapleCommand

      where StringWithMapleCommand is a string containing the command
      starting command-line Maple in the shell.
     
      This is usually "maple" on Unix machines (be sure that you do not put
      the command launching xmaple). 

      If you are using Macaulay 2 in cygwin and the Windows native Maple version
      best put the complete path to the Maple command line
      executable, e.g., StringWithMapleCommand could be (depending on the Maple version)
      
      "C:/Program Files/Maple 9.5/bin.win/cmaple9.5.exe"

      (English Windows version)
  
      "C:/Programme/Maple 9.5/bin.win/cmaple9.5.exe"
      
      (German Windows version).
      
      To test whether the interface is working do, e.g.,

      callMaple("","","returnvalue:=1;")

      (which should return 1)

  Caveat
      Note that the file init-MapleInterface.m2 will be overwritten when you reinstall a newer version of the package, but there will be a backup.

///


doc ///
  Key
    callMaple
    (callMaple,String)
    (callMaple,String,String,String)
  Headline
    Run a Maple program from Macaulay 2.
  Usage
    callMaple(mapleprogram)
    callMaple(inputdata1,inputdata2,mapleprogram)
  Inputs
    inputdata1:String
    inputdata2:String
    mapleprogram:String
  Outputs
    :Thing
  Description
   Text
        This function calls a Mapleprogram given as a string mapleprogram.

        Note that any command line there has to end with a semicolon.

        The arguments inputdata1 and inputdata2 are here just for convenience and you can put the empty strings if you want or leave them away.

        Inside mapleprogram the string placeholder1 is then replaced by the string inputdata1
        and placeholder2 is replaced the string inputdata2.

        In the string inputdata1 we take care automatically of the main compatibility problem between Macaulay 2 and Maple
        which is replacing in lists the curly brackets are by square brackets.

        The Maple program has to write its output in the Maple variable returnvalue.
        This is converted in Maple into a string, square brackets are replaced by curly brackets
        and then this string is evaluated in Macaulay 2 via the function @TO value@.

        A very simple example:
 
   Example
     L={3,5}
     mapleprogram="L:=placeholder1;returnvalue:=L[1]+L[2];";
     callMaple(toString L,"",mapleprogram)
   Text

        Here is an example how to send a @TO Matrix@ to Maple,
        compute its integer normal form and send it back to Macaulay 2:

   Example
     A=matrix {{1,5,7},{7,13,5}}
     inputdata1=toString entries A
     mapleprogram="with(linalg,ismith);A:=placeholder1;S:=ismith(A);returnvalue:=convert(S,listlist);";
     matrix callMaple(inputdata1,"",mapleprogram)
   Text

     Using inputdata2 to pass the command ismith to Maple:

   Example
     A=matrix {{1,5,7},{7,13,5}}
     inputdata1=toString entries A
     inputdata2="ismith"
     mapleprogram="with(linalg,placeholder2);A:=placeholder1;S:=placeholder2(A);returnvalue:=convert(S,listlist);";
     matrix callMaple(inputdata1,inputdata2,mapleprogram)
   Text

     (Note that it may be more convenient for you to use for mapleprogram the three-slashes string delimiter
      but this cannot be done here in the doc environment).

     Same program but obtaining also the base change matrices:

   Example
     A=matrix {{1,5,7},{7,13,5}}
     inputdata1=toString entries A
     inputdata2="ismith";
     mapleprogram="with(linalg,placeholder2):A:=placeholder1;S:=placeholder2(A,U,V);returnvalue:=[convert(S,listlist),convert(U,listlist),convert(V,listlist)];";
     L=callMaple(inputdata1,inputdata2,mapleprogram);
     S=matrix L#0
     U=matrix L#1
     V=matrix L#2
     U*A*V==S
///


doc ///
  Key
    store
    [callMaple,store]
  Headline
    Store result of a Maple computation in a file.
  Description
   Text
    If the option store=>fn with a @TO String@ fn is given
    then @TO callMaple@  stores the result of the Maple computation in a file named fn.

    This data can be read by the command @TO readMaple@.
///

doc ///
  Key
    readMaple    
    (readMaple,String)
  Headline
    Read the result of a previous Maple computation.
  Usage
    readMaple(fn)
  Inputs
    fn:String
  Outputs
    :Thing
  Description
   Text
     Read the result of a previous Maple computation stored in the file fn via the option @TO store@ of @TO callMaple@.

     @TO callMaple@( ... , @TO store@=>fn);
     readMaple(fn)

     will return the same result as

     callMaple( ... )
///

doc ///
  Key
    msqrt    
    (msqrt,ZZ,ZZ)
  Headline
    Compute modular square root.
  Usage
    msqrt(m,n)
  Inputs
    m:ZZ
    n:ZZ
  Outputs
    :Thing
        an element of ZZ or the @TO Boolean@ false.
  Description
   Text
     Computes a modular square root solving x^2 = m mod n.
     This is useful in the case of very large numbers, in which
     case the M2 native routines will not work.
   Example
     msqrt(3,12347)
///


doc ///
  Key
    integralBasis    
    (integralBasis,Ideal)
  Headline
    Compute an integral basis of an algebraic function field.
  Usage
    integralBasis(I)
  Inputs
    I:Ideal
       in a @TO PolynomialRing@ R of two variables over QQ, say x,y.
       defining an affine plane curve which has no singularities at infinity
       and its projective closure does not contain the point (x:y:z)=(0:1:0).
  Outputs
    :Matrix
        over @TO frac@(R)
  Description
   Text
     Computes an integral basis in CC(x)[y] of the integral closure of CC[x] in CC(x,y).
     We consider x as transcendental and y as algebraic. The i-th element of the
     integral basis has degree i as a polynomial in y.
     Note that the integral basis will have coefficients in QQ.

   Example
     R=QQ[x,y]
     I=ideal(y^8-x^3*(1+x)^5)
     integralBasis(I)
///



-*
uninstallPackage("MapleInterface")
installPackage("MapleInterface",RerunExamples=>true);
*-

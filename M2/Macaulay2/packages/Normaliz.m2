-- -*- coding: utf-8 -*-
newPackage(
           "Normaliz",
           Version=>"0.2",
           Date=>"March 06, 2009",
           Authors=>{{Name=> "Gesa Kaempf",
                    Email=>"gkaempf@mathematik.uni-osnabrueck.de"}},
           Headline=>"a package to use Normaliz in Macaulay 2",
           AuxiliaryFiles => true,
           CacheExampleOutput => true,
           DebuggingMode => false
           )

export{setNmzExecPath, getNmzExecPath,
       setNmzVersion,  getNmzVersion,
       setNmzFilename, getNmzFilename,
       setNmzDataPath, getNmzDataPath,
       setNmzFile,
       writeNmzPaths, 
       startNmz, 
       rmNmzFiles,
       writeNmzData, readNmzData,
       getNumInvs, showNumInvs, exportNumInvs,
       normaliz, setNmzOption, showNmzOptions,
       mons2intmat, intmat2mons,
       normalToricRing, intclToricRing, ehrhartRing, intclMonIdeal,
       torusInvariants,
       valRing, valRingIdeal
      }


-- initialising some values
nmzExecPath="";
nmzDataPath="";
nmzFilename="";
nmzUserCalled=true;  -- wether the user calls a method
nmzFile="";
nmzVersion=""; 
nmzGen=true;      -- indicates whether ".gen" is generated
numInvs={};
-- component 1 is name of option
-- 2 is default value
-- 3 is command line option to be passed to Normaliz
-- 4 indicates whether file "gen" is generated
-- value 2 of 4 indicates "no influence"

nmzOptions= new MutableList from {
            new MutableList from {"hvect",false,"-p",false},
            new MutableList from {"triang",false,"-v",false},
            new MutableList from {"supp",false,"-s",false},
            new MutableList from {"normal",false,"-n",true},
            new MutableList from {"hilb",false,"-h",true},
            new MutableList from {"dual",false,"-d",true},
            new MutableList from {"control",false,"-c",2},
            new MutableList from {"allf",false,"-a",2},
            new MutableList from {"ignore",true,"-i",2}};
-------------------------------------------------------------

--  filenames and paths

-------------------------------------------------------------

-- sets the path to the executable for normaliz
setNmzExecPath=method()
setNmzExecPath String :=stringNmzExecPath->
(
 
 if(not stringNmzExecPath=="")
 then(
     if(not stringNmzExecPath#-1=="/")
     then(
            stringNmzExecPath=stringNmzExecPath|"/";
        );
    ); 
    nmzExecPath=stringNmzExecPath;
)

-- warning: if this variable is not set, this does not mean that there is no path set in the file nmzM2Exec.path. Use startNmz to check this!
getNmzExecPath=()->
(
       return nmzExecPath;
);

-- sets the version of the executable for normaliz
setNmzVersion=method()
setNmzVersion String:=stringNmzVersion->
(
    nmzVersion=stringNmzVersion;
);


getNmzVersion=()->
(
       return nmzVersion;
)

-- sets the filename for the exchange of data
setNmzFilename=method()
setNmzFilename String :=stringNmzFilename->
(
    nmzFilename=stringNmzFilename;
    nmzFile=setNmzFile();
);


getNmzFilename=()->
(
       return nmzFilename;
)


-- sets the directory for the exchange of data
setNmzDataPath=method()
setNmzDataPath String :=stringNmzDataPath->
(
  if(not stringNmzDataPath=="")
    then(
        if(not stringNmzDataPath#-1=="/")
        then(
            stringNmzDataPath=stringNmzDataPath|"/";
        );
    ); 
    nmzDataPath=stringNmzDataPath;
);

-- warning: if this variable is not set, this does not mean that there is no path set in the file nmzM2Data.path. Use startNmz to check this!
getNmzDataPath=()->
(
       return nmzDataPath;
)

-- writes the path names into two files
writeNmzPaths=()->
(
    "nmzM2Exec.path" << nmzExecPath << close;
    "nmzM2Data.path" << nmzDataPath << close;
);


-- retrieves the path names written by writeNmzPaths 
startNmz=()->
(
    if(not fileExists("nmzM2Exec.path"))
    then error("startNmz():. First call writeNmzPaths().");

    inf:="nmzM2Exec.path";
    s:=get inf;
    L:=select(".*/",s); -- delete everything after the last /
    if(L=={})          -- this is allowed
    then(
         print "nmzExecPath not set";
    )
    else( 
           nmzExecPath=L#0;
           print("nmzExecPath is "|nmzExecPath);
   );

    if(not fileExists("nmzM2Data.path"))
    then error("startNmz(): First call writeNmzPaths().");

    inf="nmzM2Data.path";
    s=get inf; 
    L=select(".*/",s); -- delete everything after the last /
    if(L=={})          -- this is allowed
    then(
         print "nmzDataPath not set";
    )
    else( 
           nmzDataPath=L#0;
           print("nmzDataPath is "|nmzDataPath);
   );
);


-- sets the file for the exchange of data
setNmzFile=()->
( 
    if(nmzFilename!="")
    then(
        nmzFile=nmzDataPath|nmzFilename;
    )
    else
    (
     nmzFile=temporaryFileName();
    );
    return(nmzFile);
);



-- sets the version, by default it is norm64
setNmzExec=()->
(
    if(nmzVersion!="")
    then(
        nmzExec=nmzVersion;  
    )
    else
    (
        nmzExec="norm64"; 
    );
    nmzExec=nmzExecPath|nmzExec;
    return nmzExec;
);


-- removes the files created for and by normaliz
rmNmzFiles=()->
(
    suffixes:={"in","gen","out","sup","egn","esp","inv","tri","typ","hom","ext"};
    if(nmzFilename=="" and nmzUserCalled) then error("rmNmzFiles: no filename specified");
    for i from 0 to #suffixes-1 
    do(
      if(fileExists( nmzFile|"."|suffixes#i))
      then removeFile(nmzFile|"."|suffixes#i);
    );
    setNmzFilename("");
);


---------------------------------------------------------

--  parsing normaliz output (not exported)

---------------------------------------------------------

-- returns the next number in the string s, and the remaining string 
getNumber=method(TypicalValue=>(String,String))
getNumber String :=s->
(
    l:=regex("[0-9-]+",s);
    if( instance(l,Nothing)) then error("getNumber: no number found in the string.");
    if(l#0#0!=0) then error("getNumber: string must begin with a number");
    return(substring(l#0,s),substring(l#0#0+l#0#1,s));
);

-- returns the next word (marked by whitespaces) in the string s, starting at position j and replaces "_" by " ", and the position of the first whitespace
-- if s contains no whitespace, the returned position is not in the string!
getKeyword=(s,j)->
(
   l:=regex("[[:alnum:]_-]+",j,s);
   if( instance(l,Nothing)) then error("getKeyword: no word found in the string."); 
   if(l#0#0!=j) then << "warning: getKeyword: no word at the beginning of the string.";
   return(replace("_"," ",substring(l#0,s)),l#0#0+l#0#1); 
);


-- eliminates whitespaces and -, and transforms the next letter to upper case if possible
elimWhitespaces=s->
(
    tmp:="";
    while(match("[ -]",s))          -- while   or - is found
    do(
       l:=regex("[ -]",s);
       pos:=l#0#0;     -- position of   or -
       tmp=tmp|substring(0,pos,s);
       if(pos+1<#s)
       then tmp=tmp|toUpper(s#(pos+1));
       s=substring(pos+2,s); 
   );
   tmp=tmp|s;
   return tmp;
);

-------------------------------------------------------------

-- input and output to/from normaliz

-------------------------------------------------------------

-- writes the given data in a normaliz input file
doWriteNmzData=method()
doWriteNmzData(Matrix, ZZ, ZZ):=(sgr, numCols, nMode)->
(
    if(nmzFilename=="" and nmzUserCalled)  
    then error("doWriteNmzData: no filename specified"); 
    outf:=nmzFile|".in" << ""; 
    outf << numRows(sgr) << endl;
    outf << numCols << endl;

    for i from 0 to numRows(sgr)-1 
    do(
       s:="";
       for j from 0 to numCols-1 
       do(
          s=s|sgr_(i,j)|" ";
       ); 
       outf << s << endl;
    );

    outf << nMode << endl << close;
);

-- writes the given data in a normaliz input file
writeNmzData=method()
writeNmzData(Matrix,ZZ):=(sgr, nmzMode)->
(
    doWriteNmzData(sgr,numColumns(sgr), nmzMode);
);


-- reads the Normaliz output file with the specified suffix
-- suffix should not be inv, in or out 
readNmzData=method(TypicalValue=>Matrix)
readNmzData(String):=(nmzSuffix)->
(
    if(nmzSuffix=="inv" or nmzSuffix=="in" or nmzSuffix=="out") 
    then error("readNmzData: To read .inv use getNumInvs(), to read .out or .in there is no function provided");

    if(nmzFilename=="" and nmzUserCalled) then error("readNmzData: no filename specified");

    if(not fileExists(nmzFile|"."|nmzSuffix))
    then( 
        error("readNmzData: No file "|nmzFile|"."|nmzSuffix|" found. Perhaps you need to activate another option.");
    );

    inf:=get(nmzFile|"."|nmzSuffix);
    s=lines(inf);
    nmzGen:={};
    i:=2;
    t:="";
    while(i<#s)
    do( 
       t = select("[0-9-]+",s#i);
       gen:=apply(t,value);
       nmzGen=append(nmzGen,gen);
       i=i+1;
    );
    if(nmzGen!={})
    then  return(matrix(nmzGen))
    else return;   -- should not appear unless the user calls it
);

-------------------------------------------------------------

-- retrieving normaliz numerical invariants

-------------------------------------------------------------
getNumInvs=()->
(
  return numInvs;
)

doGetNumInvs=()->
(
    numInvs:={};
    key:="";
    inv:=0;

    if(not fileExists(nmzFile|".inv"))
    then error("doGetNumInvs(): No file "|nmzFile|".inv"|" found.");

    inf:=get(nmzFile|".inv");  
    s:=lines(inf);

    for i from 0 to #s-1   -- for each line in the file
    do(
       key="";

       if(match("^integer", s#i))
       then( 
            (key,j)=getKeyword(s#i,8); 
             inv=(getNumber(substring(j+3,s#i)))#0;
       )
      else( if(match("^boolean",s#i))
            then(
                 (key,j)=getKeyword(s#i,8);
                  if(s#i#(j+3)=="t")
                  then( inv="true";)
                  else( inv="false";);
            )
            else (if(match("^vector",s#i))
                  then(
                       (len,str):=getNumber(substring(7,s#i)); 
                       (key,j)=getKeyword(str,1);
                       inv={}; 
                        t:= replace(".* = ","",s#i);
                        u:= select("[0-9-]+",t);
                        inv=toSequence(apply(u,value));
                  );
           );
      );


    numInvs=append(numInvs,{key,inv});
    );
    return(numInvs);
);

-- types the numerical invariants on the standard output
showNumInvs=()->
(
    l:=getNumInvs();
    apply(l,(p)->(print(p#0|" : "|toString(p#1))));
);

-- makes the numerical invariants in the .inv file available to Macaulay 2, and prints them to the standard output, if the print option is true
opts={Print => false}
exportNumInvs=opts >> o->()->
(
    l:=getNumInvs();
    apply(l,(p)->(
                  value("nmz"|elimWhitespaces(" "|p#0)|"="|toString(p#1));
                  if(o.Print)
                  then(
                  print ("nmz"|elimWhitespaces(" "|p#0)|"="|toString(p#1));)));
)
----------------------------------------------------------

-- running normaliz (with options)

----------------------------------------------------------

setNmzOption=method()
setNmzOption (String,Boolean):=(s, onoff)->
(
  i:=position(toList(nmzOptions),((x)->(return x#0===s;)));

  if(instance(i,Nothing))
  then(
       print("setNmzOption: Invalid option "| s);
       return(false);
  )
  else(
       nmzOptions#i#1=onoff;
       return(true);
  );
)

collectNmzOptions=()->
(
   options:=" -f ";
    for i from 0 to #nmzOptions-1
    do(
        if(nmzOptions#i#1)
        then(
            options=options|nmzOptions#i#2|" ";
            if(nmzOptions#i#3=!=2)
            then(
                nmzGen=nmzOptions#i#3;
            );
        );
    );
    return(options);
)

showNmzOptions=()->
(
  << "The following options are set:"<< endl;
  << collectNmzOptions();
)


runNormaliz=method()
runNormaliz(Matrix,ZZ,ZZ):=(sgr,numCols, nmzMode)->
( 
    nmzFile=setNmzFile();

    nmzUserCalled=false;
    doWriteNmzData(sgr,numCols,nmzMode);
    options:=collectNmzOptions();
 
    cmd:="";
    dir:=select(".*/",nmzFile);
    if(dir!={}) then cmd="cd "|dir#0|";";

    cmd = cmd|setNmzExec()|options|baseFilename(nmzFile);
    if debugLevel > 0 then << "--running command: " << cmd << endl;
    if 0 != run cmd then error ("command failed : ", cmd);
    if debugLevel > 0 then << "--command succeeded" << endl;

    if(not nmzGen)  -- return nothing if .gen is not
    then(            -- generated
         if(nmzFilename=="") 
         then( 
              rmNmzFiles(); 
         );
         nmzUserCalled=true;  -- back to default
         return; 
       );

    M:=readNmzData("gen");
    numInvs=doGetNumInvs();
    if(nmzFilename=="") 
    then(
         rmNmzFiles();
    );
    nmzUserCalled=true;  -- back to default
    return(M);
);


normaliz=method()
normaliz(Matrix,ZZ):=(sgr,nmzMode)->
(
    return(runNormaliz(sgr,numColumns(sgr),nmzMode));
);

-------------------------------------------------------------

-- intmats to/from monomials

-------------------------------------------------------------

mons2intmat=method()
mons2intmat Ideal :=I->
(

    mat:={};
    v:={};
    g:=gens I;     -- matrix with one row
    for i from 0 to numColumns(g)-1 
    do(
       v=(exponents(leadMonomial(g_(0,i))))#0;
       mat=append(mat,v);
    );
    return(matrix(mat));
);

-- expos: a matrix whose numColumns is <= numgens(r)
-- r: the ring where the ideal shall be
-- returns the ideal
intmat2mons=method()
intmat2mons(Matrix,Ring):=(expoVecs, r)->
(
   if(numColumns(expoVecs)< numgens(r))
   then(
        error("intmat2mons: not enough variables in the basering");
   );

   v:=vars(r);  -- the variables of the basering, a matrix with one row
   l:={};

   for i from 0 to numRows(expoVecs)-1
   do(
      m:=1;
      for j from 0 to numColumns(expoVecs)-1
      do(
         m=m*(v_(0,j))^(expoVecs_(i,j));
      );
      l=append(l,m);
   );
 
   return(ideal(l));

);

-- takes only the rows with last entry d
intmat2mons(Matrix,Ring,ZZ):=(expoVecs,r,d)->
(
   if(numColumns(expoVecs)< numgens(r))
   then(
        error("intmat2mons: not enough variables in the basering");
   );
   v:=vars(r);  -- the variables of the basering, a matrix with one row
   l:={};

   for i from 0 to numRows(expoVecs)-1 
   do(
      if(expoVecs_(i,numColumns(expoVecs)-1)==d)
      then (
             m:=1;
             for j from 0 to numColumns(expoVecs)-2 
             do(
                 m=m*(v_(0,j))^(expoVecs_(i,j));
             );
             l=append(l,m);
      );
   );
   return(ideal(l));

);

-------------------------------------------------------------

-- integral closure of rings and ideals

-------------------------------------------------------------

runIntclToricRing=method()
runIntclToricRing(Ideal,ZZ):=(I,nmzMode)->
(
    expoVecs:=mons2intmat(I);

     res:=runNormaliz(expoVecs,numColumns(expoVecs),nmzMode);
     if(instance(res,Nothing))
     then return
     else return(intmat2mons(res,ring(I)));
);

intclToricRing=method()
intclToricRing Ideal :=I->
(
    return(runIntclToricRing(I,0));
);

normalToricRing=method();
normalToricRing Ideal := I->
(
    return(runIntclToricRing(I,1));
);


runIntclMonIdeal=method()
runIntclMonIdeal(Ideal,ZZ):=(I,nmzMode)->
(
    expoVecs:=mons2intmat(I);
    lastComp:=0;

    -- we test if there is room for the Rees algebra

    for i from 0 to numRows(expoVecs)-1
    do(
        if(not expoVecs_(i,numColumns(expoVecs)-1)==0)
        then(
            lastComp=1;  break; -- no
        );
    ); 

    nmzData:=runNormaliz(expoVecs,numColumns(expoVecs)-1+lastComp,nmzMode);
    
    if(not nmzGen) then return;

    if(lastComp==1)
    then(
         I1=intmat2mons(nmzData,ring(I),1); 
         return({I1});  
    )
    else
    (
        I1=intmat2mons(nmzData,ring(I),1);
        I2=intmat2mons(nmzData,ring(I));
       return({I1,I2});
    );
);


ehrhartRing=method()
ehrhartRing Ideal :=I->
(
    return(runIntclMonIdeal(I,2));
);

intclMonIdeal=method()
intclMonIdeal Ideal :=I->
(
    return(runIntclMonIdeal(I,3));
);

--------------------------------------------------------

-- torus invariants and valuation rings and ideals

--------------------------------------------------------
torusInvariants=method()
torusInvariants (Matrix, Ring) :=(T,R)->
(

    if(numgens(R)!=numColumns(T))
    then(
          error("torusInvariants: wrong number of columns in matrix");
    );

    M:=runNormaliz(T,numColumns(T),5);
    if(not nmzGen) then return;  -- M=null

    return(intmat2mons(M,R) );
)

valRing=method()
valRing (Matrix,Ring) :=(V,R)->
(
    if(numgens(R)!=numColumns(V))
    then(
          error("valRing: wrong number of columns in matrix");
    );

    I:=id_(ZZ^(numColumns(V))); -- identity matrix
    V1:=I||V;

    M:=runNormaliz(V1,numColumns(V),4);
 
    if(not nmzGen) then return; -- M=null

    return(intmat2mons(M,R)); 
)

valRingIdeal=method()
valRingIdeal (Matrix,Ring):=(V,R)->
(
    nc:=numColumns(V);
    if(numgens(R)!=nc-1)
    then(
         error("valRingIdeal: wrong number of columns in matrix");
    );

    I:=id_(ZZ^nc); -- identity matrix
    V1:=I||V;
    V1=mutableMatrix(V1);
    
    for i from 0 to numRows(V)-1
    do(
       V1_(i+nc,nc-1)=-V1_(i+nc,nc-1);
    );
    V1=matrix(V1);

    M:=runNormaliz(V1,nc,4);
    if(not nmzGen) then return; -- M=null

    I1:=intmat2mons(M,R,0);
    I2:=intmat2mons(M,R,1);
    return({I1,I2}); 
)

----------------------------------------------------------
beginDocumentation()

document {
     Key => Normaliz,
     Headline => "an interface to use Normaliz in Macaulay 2",
     "The package ", EM "Normaliz"," provides an interface for the use of ", TT "Normaliz 2.1"," within Macaulay 2. The exchange of data is via files, the only possibility offered by ", TT "Normaliz"," in its
present version. In addition to the top level functions that aim at objects of type ", TO "Ideal"," or ", TO "Ring",  ", several other auxiliary functions allow the user to apply ", TT "Normaliz"," to data of type ", TO "Matrix","."  }



document {
     Key => {setNmzExecPath, (setNmzExecPath,String)},
     Headline => "sets the path to the executable for Normaliz",
     Usage => "setNmzExecPath(s)",
     Inputs => {
           String => "a string containing the path" 
          },
     Consequences => {
          {"The function stores ", TT "s", " in the global variable holding the path name."}
         },
     "This is absolutely necessary if it is not in the search path. Note that the string should not contain $ since Macaulay 2 seems to have problems with such paths.",
    EXAMPLE lines ///
        setNmzExecPath("~/Normaliz2.1Linux");  -- Unix
        setNmzExecPath("d:/Normaliz2.1Windows"); -- Windows
        getNmzExecPath() 
        setNmzExecPath("")  --reset
        ///
        ,
     {"The last ", TT "/", " is added if necessary."},
     SeeAlso => getNmzExecPath,
     }

document {
     Key => {getNmzExecPath},
     Headline => "returns the path to the executable for Normaliz",
     Usage => "getNmzExecPath()",
     Outputs => {
           String => "the string containing the path"
          },
      "The default value is the empty string.",
    EXAMPLE lines ///
        getNmzExecPath()
        ///,
     Caveat =>{"This is the value stored in the global variable. The function ", TO startNmz, " retrieves the path names written by ", TO writeNmzPaths, " to the hard disk, so this can be a different path." },
     SeeAlso=>setNmzExecPath
     }

document {
     Key => {setNmzVersion, (setNmzVersion,String)},
     Headline => "sets the version of the executable for Normaliz",
     Usage => "setNmzVersion(s)",
     Inputs => {
           String => {"should be one of the following: ", TT "norm32", ", ", TT "norm64", ", or ", TT "normbig" }
          },
     "The default is ", TT "norm64",".",
    EXAMPLE lines ///
        setNmzVersion("normbig"); 
        getNmzVersion()
        setNmzVersion("norm32"); 
        ///
        ,
     SeeAlso => getNmzVersion
     }


document {
     Key => getNmzVersion,
     Headline => "returns the current version of Normaliz to be used",
     Usage => "getNmzVersion()",
     Outputs => {
          String => "the current version of Normaliz to be used"
          },
     EXAMPLE lines ///
          getNmzVersion()
          setNmzVersion("normbig");
          getNmzVersion()
          ///,
     SeeAlso => setNmzVersion,
     }

document {
     Key => {setNmzFilename, (setNmzFilename,String)},
     Headline => "sets the filename for the exchange of data",
     Usage => "setNmzFilename(s)",
     Inputs => {
           String => "the filename for the exchange of data"
          },
     Consequences => {"stores the input string in the global variable holding the filename"},
     {"If the input is the empty string, a temporary filename in a temporary directory will be created, but the global variable still stores the empty string.",BR{}, "Only if the user specifies a (non-empty) filename, the files created by ",TT"Normaliz"," are not removed."},
    EXAMPLE lines ///
        getNmzFilename()
        setNmzFilename("VeryInteresting");
        getNmzFilename()
        setNmzFile()
        setNmzFilename(""); 
        getNmzFilename() 
        setNmzFile()
        ///
        ,
     SeeAlso => {getNmzFilename,setNmzFile}
     }


document {
     Key => getNmzFilename,
     Headline => "returns the current filename specified by the user",
     Usage => "getNmzFilename()",
     Outputs => {
          String => "the current filename specified by the user"
          },
     "The default value is the empty string.",
     EXAMPLE lines ///
        getNmzFilename()
        setNmzFilename("VeryInteresting");
        getNmzFilename()
        setNmzFilename(""); 
        getNmzFilename() 
          ///,
     SeeAlso => setNmzFilename
     }

document {
     Key => {setNmzDataPath, (setNmzDataPath,String)},
     Headline => "sets the directory for the exchange of data",
     Usage => "setNmzDataPath(s)",
     Inputs => {
           String => "the directory for the exchange of data"
          },
     {"By default it is the current directory. The function does not check if the directory exists. If it does not exist, ", TT  "Normaliz"," will issue an error message. Use e.g. ", TO makeDirectory," to create an directory within Macaulay 2.",    BR{},"Note that the path should not contain $ since Macaulay 2 seems to have problems with such paths. If no filename is specified, this path is not used."},
    EXAMPLE lines ///
        setNmzDataPath("d:/Normaliz2.1Windows/example"); 
        getNmzDataPath() 
        ///
        ,
     SeeAlso => getNmzDataPath,
     }


document {
     Key => getNmzDataPath,
     Headline => "returns the directory for the exchange of data",
     Usage => "getNmzDataPath()",
     Outputs => {
          String => "the path to the directory to be used for the exchange of data"
          },
     EXAMPLE lines ///
          getNmzDataPath()
          setNmzDataPath("d:/Normaliz2.1Windows/example");
          getNmzDataPath()
          ///,
     Caveat =>{"This is the value stored in the global variable. The function ", TO startNmz, " retrieves the path names written by ", TO writeNmzPaths, " to the hard disk, so this can be a different path." },
     SeeAlso => setNmzDataPath
     }

document {
     Key => setNmzFile,
     Headline => "sets the filename for the exchange of data",
     Usage => "setNmzFile()",
     Outputs => {
          String => "the path and the filename"
          },
     "If a non-empty filename is specified by the user, this function returns the concatenated data path (if specified by ", TO setNmzDataPath,") and the filename. If no filename is specified, a temporary filename is created. In this case the files created by ", TT "Normaliz", " are removed automatically.",
     EXAMPLE lines ///
        setNmzFilename("VeryInteresting");
        setNmzFile()
        setNmzFilename("");
        nmzFile=setNmzFile()
          ///,
     }


document {
     Key => writeNmzPaths,
     Headline => "writes the path names into two files",
     Usage => "writeNmzPaths()",
     "This function writes the path names into two files in the current directory. If one of the names has not been defined, the corresponding file is written, but contains nothing.",
     EXAMPLE lines ///
          writeNmzPaths();
          ///,
     SeeAlso => startNmz
     }

document {
     Key => startNmz,
     Headline => "initializes a session for Normaliz",
     Usage => "startNmz()",
     "This function reads the files written by ", TO "writeNmzPaths", ", retrieves the path names, and types them on the standard output (as far as they have been set). Thus, once the path names have
been stored, a ", TT "Normaliz", " session can simply be opened by this function.", 
     EXAMPLE lines ///
          writeNmzPaths();
          startNmz();
          ///,
     SeeAlso => writeNmzPaths
     }

document {
     Key => rmNmzFiles,
     Headline => "removes the files created by Normaliz",
     Usage => "rmNmzFiles()",
     "This function removes the files created for and by ", TT "Normaliz", ", using the last filename created. These files are removed automatically unless a (non-empty) filename has been specified using ", TO setNmzFilename,". In this case the filename is reset to the empty string.",
     EXAMPLE lines ///
          setNmzFilename("VeryInteresting");
          R=ZZ/37[x,y,z];
          I=ideal(x^2*y, y^3);
          normalToricRing(I);
          get (setNmzFile()|".typ")
          rmNmzFiles();
          getNmzFilename()
          ///,
     }

document {
     Key => {writeNmzData, (writeNmzData, Matrix, ZZ)},
     Headline => "creates an input file for Normaliz",
     Usage => "writeNmzData(sgr, nmzMode)",
     Inputs =>{
                Matrix => "generators of the semigroup",
                ZZ => "the mode"
      },
      Consequences => {"an input file filename.in is written, using the last filename created"}, 
     "This function creates an input file for ", TT "Normaliz", ". The rows of ", TT "sgr", " are considered as the generators of the semigroup. The parameter ", TT "nmzMode"," sets the mode. If no filename has been specified, an error occurs.",
     EXAMPLE lines ///
          setNmzFilename("example"); -- to keep the files
          sgr=matrix({{1,2,3},{4,5,6},{7,8,10}})
          writeNmzData(sgr,1)
          get(setNmzFile()|".in")
          rmNmzFiles();
          ///,
     SeeAlso => readNmzData
     }

document {
     Key => {readNmzData, (readNmzData, String)},
     Headline => "reads an output file of Normaliz",
     Usage => "readNmzData(s)",
     Inputs => {
                String => "the suffix of the file to be read" 
     },
     Outputs => {
                 Matrix => " the content of the file"
     },
     "Reads an output file of ", TT "Normaliz", " containing an integer matrix and returns it as an ", TO "Matrix", ". For example, this function is useful if one wants to inspect the support hyperplanes. The filename is
     created from the current filename specified by the user and the suffix given to the function.",
     EXAMPLE lines ///
         setNmzFilename("example") -- to keep the files
         sgr=matrix({{1,2,3},{4,5,6},{7,8,10}});
         normaliz(sgr,0)
         readNmzData("sup")
         rmNmzFiles();
          ///,
     SeeAlso => {writeNmzData, normaliz}
     }

document {
     Key => {normaliz, (normaliz, Matrix, ZZ)},
     Headline => "applies Normaliz",
     Usage => "normaliz(sgr,nmzMode)",
     Inputs => {
                Matrix => " the matrix",
                ZZ => " the mode"
     },
     Outputs => {Matrix => "generators of the integral closure"},
     "This function applies ", TT "Normaliz", " to the parameter ", TT "sgr", " in the mode set by ", TT "nmzMode", ". The function returns the ", TO Matrix, " defined by the file with suffix ", TT "gen", " , if computed.",
     EXAMPLE lines ///
         sgr=matrix({{1,2,3},{4,5,6},{7,8,10}});
         normaliz(sgr,0)
          ///,
     SeeAlso => readNmzData
     }


document {
     Key => {getNumInvs},
     Headline => "returns the numerical invariants computed",
     Usage => "getNumInvs()",
     Outputs => {List => "the numerical invariants"},
     "This function returns a list whose length depends on the invariants available. The order of the elements in the list is always the same. Each list element has two parts. The first is a ", TO String, " describing the invariant, the second is the invariant, namely an ", TO ZZ, " for rank, index, multiplicity, a ", TO Sequence, " for the weights, the h-vector and the Hilbert polynomial and a ", TO Boolean, " for homogeneous and primary."
     ,
     EXAMPLE lines ///
          R=ZZ/37[x,y,t];
          I=ideal(x^3,x^2*y,y^3);
          setNmzOption("hilb",true);
          intclMonIdeal(I);
          getNumInvs()
          ///,
     {"Note: This function is available even if no filename has been specified."},
     SeeAlso => {showNumInvs, exportNumInvs}
     }

document {
     Key => {showNumInvs},
     Headline => "prints the numerical invariants computed",
     Usage => "showNumInvs()",
     "This function types the numerical invariants on the standard output, but returns nothing. (It calls ", TO getNumInvs, ".)"
     ,
     EXAMPLE lines ///
          R=ZZ/37[x,y,t];
          I=ideal(x^3,x^2*y,y^3);
          setNmzOption("hilb",true);
          intclMonIdeal(I);
          showNumInvs()
          ///,
     {"Note: This function is available even if no filename has been specified."},
     SeeAlso => {getNumInvs, exportNumInvs}
     }

document {
     Key => {exportNumInvs},
     Headline => "makes the numerical invariants availabe to Macaulay 2",
     Usage => "exportNumInvs()",
     "This function exports the data read by ", TO getNumInvs, " into numerical ", TT "Macaulay 2", " data that can be accessed directly. For each invariant a variable of type ", TO ZZ, " or ", TO Sequence, " is created whose name is the first entry of each list element shown above, prefixed by ", TT "nmz", ". The variables created and their values are printed to the standard output."
     ,
     EXAMPLE lines ///
          R=ZZ/37[x,y,t];
          I=ideal(x^3,x^2*y,y^3);
          setNmzOption("hilb",true);
          intclMonIdeal(I);
          exportNumInvs()
          nmzHilbertBasisElements
          nmzHomogeneousWeights
          ///,
     {"Note: This function is available even if no filename has been specified."},
     SeeAlso => {getNumInvs, showNumInvs}
     }

document {
     Key => [exportNumInvs, Print],
     Headline => "wether to print the variables",
     Usage => "exportNumInvs(Print => true)",
     Inputs => {
               Boolean => "wether to print the variables created."
          },
     "If the ", TT "Print", " option is set to true, the function does not only create the variables, but also prints them to the standard output. The default is ", TT "false", ".",
     EXAMPLE lines ///
          R=ZZ/37[x,y,t];
          I=ideal(x^3,x^2*y,y^3);
          setNmzOption("hilb",true);
          intclMonIdeal(I);
          exportNumInvs()
          exportNumInvs(Print=> true)
          ///,
     }

document {
     Key => {setNmzOption,(setNmzOption,String,Boolean)},
     Headline => "set an option",
     Usage => "setNmzOption(s,b)",
     Inputs => {
               String => "name of the option",
               Boolean => "true switches the option on, false off"
          },
     {"The ", TT "Normaliz"," options are accessible via the following names: ", BR{},BR{},
     "Run mode type:",BR{},BR{},
     TT "-s",":   supp",BR{},
     TT "-v",":   triang",BR{},
     TT "-p",":   hvect",BR{},
     TT "-n",":   normal",BR{},
     TT "-h",":   hilb",BR{},
     TT "-d",":   dual",BR{},BR{},
     "Further options:",BR{},BR{},
     TT "-c",":   control" ,BR{}, 
     TT "-a",":   allf",BR{},
     TT "-i",":   ignore",BR{},BR{},
     },
     {"Note that it makes no sense to activate more than one of the run mode options. The ", TT "-f", " option is always set. The default value of all options is ", TT "false", " except for ", TT"ignore","."},
     EXAMPLE lines ///
          setNmzOption("triang",true);
          showNmzOptions()
               ///,
     SeeAlso => showNmzOptions
     }

document {
     Key => {showNmzOptions},
     Headline => "prints the enabled options",
     Usage => "showNmzOptions()",
     "Prints the enabled options to the standard output. The ", TT "-f", " option is always set.",
     EXAMPLE lines ///
          setNmzOption("triang",true);
          showNmzOptions()
          ///,
     SeeAlso => setNmzOption
     }

document {
     Key => {mons2intmat, (mons2intmat, Ideal)},
     Headline => "matrix of leading exponents",
     Usage => "mons2intmat(I)",
     Inputs => { Ideal => "the ideal"},
     Outputs => {Matrix => {" rows represent the leading exponents of the generators of ", TT "I"}},
    "This function returns the ",TO Matrix, " whose rows represent the leading exponents of the elements of ", TT "I", ". The length of each row is the numbers of variables of the ambient ring of ", TT " I", ".",
     EXAMPLE lines ///
          R=ZZ/37[x,y,t];
          J=ideal(x^3, x^2*y, y^3, x*y^2,x*y^2*t^7);
          m=mons2intmat(J)
          I=intmat2mons(m,R)
          I==J
          ///,
     SeeAlso => {intmat2mons}
     }

document {
     Key => { intmat2mons, (intmat2mons, Matrix, Ring)},
     Headline => "monomials from a matrix",
     Usage => "intmat2mons(m,R)",
     Inputs => { Matrix => "a matrix, whose rows represent the exponent vectors",
                 Ring => "the ring, whose elements the monomials shall be"},
     Outputs => {Ideal => {"an ideal in ", TT "R", " generated by the monomials in ", TT "R", " whose exponent vectors are given by ", TT "m", "."}},
    " This functions interprets the rows of the matrix ", TT "m", " as exponent vectors of monomials in ", TT "R", ". It returns the ideal generated by these monomials.",
     EXAMPLE lines ///
          R=ZZ/37[x,y,t];
          m=matrix({{3,0,0},{2,1,0},{0,3,0},{1,2,7}})
          I=intmat2mons(m,R)
          n=mons2intmat(I)
          m==n
     ///,
     SeeAlso => {mons2intmat}
     }

document {
     Key => { (intmat2mons, Matrix, Ring,ZZ)},
     Headline => "monomials from a matrix",
     Usage => "intmat2mons(m,R,d)",
     Inputs => { Matrix => "a matrix, whose rows represent the exponent vectors",
                 Ring => "the ring, whose elements the monomials shall be",
                 ZZ => "takes only the rows with last entry d"},
     Outputs => {
                 Ideal => {"an ideal in ", TT "R", " generated by the monomials in ", TT "R", " whose exponent vectors are given by the rows of ", TT "m", "  whose last entry is ", TT"d","."}},
    " This functions interprets the rows of the matrix ", TT "m", " as exponent vectors of monomials in ", TT "R", ". It returns the ideal generated by these monomials.",
     EXAMPLE lines ///
          R=ZZ/37[x,y,t];
          m=matrix({{3,0,0},{2,1,0},{0,3,0},{1,2,7}})
          I=intmat2mons(m,R)
          J=intmat2mons(m,R,0)
     ///,
     SeeAlso => {mons2intmat, intmat2mons}
     }

document {
     Key => {normalToricRing, (normalToricRing, Ideal)},
     Headline => "normalization of a toric ring",
     Usage => "normalToricRing(I)",
     Inputs => {Ideal => "the leading monomials of the elements of the ideal generate the toric ring"},
     Outputs => {Ideal => "the generators of the ideal are the generators of the normalization"},
    "This function computes the normalization of the toric ring generated by the leading monomials of the elements of ", TT "I", ". The function returns an ", TO Ideal, " listing the generators of the normalization.",BR{},BR{}, EM "A mathematical remark:", " the toric ring (and the other rings computed) depends on the list of monomials given, and not only on the ideal they generate!",
     EXAMPLE lines ///
          R=ZZ/37[x,y,t];
          I=ideal(x^3, x^2*y, y^3, x*y^2);
          normalToricRing(I)
     ///,
     }

document {
     Key => {intclToricRing, (intclToricRing, Ideal)},
     Headline => "integral closure of a toric ring",
     Usage => "intclToricRing(I)",
     Inputs => {
                Ideal => "the leading monomials of the elements of the ideal generate the toric ring"},
     Outputs => { 
                 Ideal => "the generators of the ideal are the generators of the integral closure"},
    "This function computes the integral closure of the toric ring generated by the leading monomials of the elements of ", TT "I", ". The function returns an ", TO Ideal, " listing the generators of the integral closure.",BR{},BR{}, EM "A mathematical remark:", " the toric ring (and the other rings computed) depends on the list of monomials given, and not only on
the ideal they generate!",
     EXAMPLE lines ///
          R=ZZ/37[x,y,t];
          I=ideal(x^3, x^2*y, y^3, x*y^2);
          intclToricRing(I)
     ///,
     }

document {
     Key => {ehrhartRing, (ehrhartRing, Ideal)},
     Headline => "Ehrhart ring",
     Usage => "ehrhartRing(I)",
     Inputs => {Ideal => "the leading monomials of the elements of the ideal are considered as generators of a lattice polytope"},
     Outputs => {List => " a list containing one or two ideals"}, 
     "The exponent vectors of the leading monomials of the elements of ", TT "I", " are considered as generators of a lattice polytope. The function returns a list of ideals:", BR{}, BR{}, EM "(i)"," If the last ring variable is not used by the monomials, it is treated as the auxiliary variable of the Ehrhart ring. The function returns two ideals, the first containing the monomials representing the lattice points of the polytope, the second containing the generators of the Ehrhart ring.", BR{},BR{}, EM "(ii)", " If the last ring variable is used by the monomials, the function returns only one ideal, namely the monomials representing the lattice points of the polytope.",

     EXAMPLE lines ///
          R=ZZ/37[x,y,t];
          I=ideal(x^3, x^2*y, y^3, x*y^2);
          ehrhartRing(I)
          J=I+ideal(x*y^2*t^7);
          ehrhartRing(J)
     ///,
     }

document {
     Key => { intclMonIdeal, (intclMonIdeal, Ideal)},
     Headline => "Rees algebra",
     Usage => "intclMonIdeal(I)",
     Inputs => {Ideal => "the leading monomials of the elements of the ideal are considered as generators of a monomial ideal whose Rees algebra is computed"},
     Outputs => {List => " a list containing one or two ideals"},
     "The exponent vectors of the leading monomials of the elements of ", TT "I"," are considered as generators of a monomial ideal whose Rees algebra is computed. The function returns a list of ideals:", BR{},BR{}, EM "(i)", " If the last ring variable is not used by the monomials, it is treated as the auxiliary variable of the Rees algebra. The function returns two ideals, the first containing the monomials generating the integral closure of the monomial ideal, the second containing the generators of the Rees algebra. ", BR{},BR{},EM "(ii)", " If the last ring variable is used by the monomials, it returns only one ideal, namely the monomials generating the integral closure of the ideal.",
     EXAMPLE lines ///
          R=ZZ/37[x,y,t];
          I=ideal(x^3, x^2*y, y^3, x*y^2);
          intclMonIdeal(I)
          J=I+ideal(x*y^2*t^7);
          intclMonIdeal(J)
     ///,
     }

document {
     Key => { torusInvariants, (torusInvariants, Matrix,Ring)},
     Headline => "ring of invariants",
     Usage => "torusInvariants(T,R)",
     Inputs => {
                Matrix=> {"matrix ", TEX "(a_{ij})", " of the action"},
                Ring => " the ring on which the action takes place" 
     },
     Outputs => {
                 Ideal => {"the list of monomials generating the ring of invariants ",TEX "R^T"}
     },
     {" Let ", TEX "T=(K^*)^r"," be the ", TEX "r","-dimensional torus acting on the polynomial ring ",TEX "R=K[X_1,\\ldots,X_n]"," diagonally. Such an action can be described as follows: there are integers ",TEX "a_{ij}, i=1,\\ldots,r, j=1,\\ldots,n",", such that ",TEX "(\\lambda_1,\\ldots,\\lambda_r)\\in T"," acts by the substitution ",BR{},BR{}, TEX "X_j\\mapsto \\lambda_1^{a_{1j}}*\\ldots*\\lambda_r^{a_{rj}}X_j,", "    ", TEX "j=1,\\ldots,n.", BR{},BR{},"In order to compute the ring of invariants ",TEX "R^T",", one must specify the matrix ", TEX "(a_{ij})."},
     EXAMPLE lines ///
          R=QQ[x,y,z,w];
          T=matrix({{-1,-1,2,0},{1,1,-2,-1}});
          torusInvariants(T,R)
     ///,
     Caveat => {"It is of course possible that ", TEX "R^T=K",". At present, ", TT "Normaliz cannot deal with the zero cone and will issue the (wrong) error message that the cone is not pointed."},
     SeeAlso => {valRing, valRingIdeal}
     }

document {
     Key => { valRing, (valRing,Matrix,Ring)},
     Headline => "ring of valuations",
     Usage => "valRing(v,r)",
     Inputs => {
                Matrix=> "values of the indeterminates",
                Ring => " the basering" 
},
     Outputs => { 
                 Ideal => {"the list of monomials generating the subalgebra of elements with valuation ",TEX "\\geq 0"}},
     {"A discrete monomial valuation ", TEX "v"," on ", TEX "R=K[X_1,\\ldots,X_n]"," is determined by the values ", TEX "v(X_j)"," of the indeterminates. This function computes the subalgebra ", TEX "S=\\{f\\in R: v_i(f)\\geq 0, i=1,\\ldots,n\\}"," for several such valuations ", TEX "v_i, i=1,\\ldots,r",". The function needs the matrix ", TEX "V=(v_i(X_j))"," as its input."},
     EXAMPLE lines ///
         R=QQ[x,y,z,w];
         V0=matrix({{0,1,2,3},{-1,1,2,1}});
         valRing(V0,R)
     ///,
     Caveat => {"It is of course possible that ", TEX "S=K",". At present, ", TT "Normaliz"," cannot deal with the zero cone and will issue the (wrong) error message that the cone is not pointed."},
     SeeAlso => {valRingIdeal, torusInvariants}
     }

document {
     Key => { valRingIdeal, (valRingIdeal,Matrix,Ring)},
     Headline => "valuation ideal",
     Usage => "valRingIdeal(v,r)",
     Inputs => {
                Matrix=> {"values of the indeterminates, the last column contains the lower bounds ", TT "w_i"},
                Ring => " the basering" 
     },
     Outputs => {
                 List => "a list of two ideals"},
     {"A discrete monomial valuation ", TEX "v"," on ", TEX "R=K[X_1,\\ldots,X_n]"," is determined by the values ", TEX "v(X_j)"," of the indeterminates. The function returns two ideals, both to be considered as lists of monomials. The first is the system of monomial generators of the subalgebra ", TEX "S=\\{f\\in R: v_i(f)\\geq 0, i=1,\\ldots,n\\}"," for several such valuations ", TEX "v_i, i=1,\\ldots,r",", the second the system of generators of the submodule ", TEX "M=\\{f\\in R: v_i(f)\\geq w_i, i=1,\\ldots,n\\}"," for integers ", TEX "w_1,\\ldots,w_r","."},
     EXAMPLE lines ///
           R=QQ[x,y,z,w]; 
           V=matrix({{0,1,2,3,4},{-1,1,2,1,3}});
           valRingIdeal(V,R)
     ///,
     Caveat => {"It is of course possible that ", TEX "S=K",". At present, ", TT "Normaliz"," cannot deal with the zero cone and will issue the (wrong) error message that the cone is not pointed."},
     SeeAlso=> {valRing, torusInvariants}
     }

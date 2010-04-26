loadPackage "ExampleIdeals"
EGS = getExampleFile "gb-one-minute.m2"

alg = (header,str) -> (
     test'code = null;
     assert'code = null;
     I := value str;
     t1 := timing(J = gens gb(I, Algorithm=>LinearAlgebra));
     if test'code =!= null then value test'code;
     if assert'code =!= null then value assert'code;
     toString t1#0|" sec "|header
     )

runexample = (i) -> (
     masterfile = openOutAppend "master-f4";
     << "example " << i << " " << flush;
     s := alg(EGS#i#0, EGS#i#1);
     << "example" << i << " " << s << endl << flush;
     masterfile << i << " " << s << endl << flush;
     close masterfile
     )

getsvn = () -> replace("Revision: ", "r",first lines get "!svn info | grep Revision")
getdate = () -> first lines get "!date"
whoami = () -> first lines get "!whoami"
getarch = () -> (
     s := first lines get "!uname -rs";
     if match("Darwin", s) and whoami() == "mike"  then s = "MBP 2.4 GHz Core 2 Duo 4 GB RAM\n-- "|s;
     s
     )
header = () -> "-- "|getsvn()|" -- "|getarch()|" -- "|getdate()     

if class eg === ZZ then (
  runexample eg;
  exit 0;
  ) else (
     masterfile = openOutAppend "master-f4";
     masterfile << endl;
     masterfile << header() << endl;
     close masterfile;
     for i in keys EGS do run ("M2 -q -e eg="|toString i|" run-gb-one-minute.m2");
     << "done running examples" << endl;
     )
end

restart
load "run-gb-one-minute.m2"

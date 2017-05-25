ID=" $OpenXM: OpenXM/src/Macaulay2/m2/curl.m2,v 1.2 2015/08/19 07:20:27 takayama Exp $ "

-- curl interface for M2
-- BSD license without advertisements

curl = method()
curl(String,String) := (s,url) -> (
     try (F := get ("!curl " | s | " " | url))
     else ( error "Could not start curl " | s | " " | url);
     F
     )

test1 = method()
test1(ZZ) := (ttype) -> (
   ans1=
   curl("--data 'oxMessageBody=fd_hessian2(-2,[-3,-2],10,[1/2,1/3])'",
        "http://asir2.math.kobe-u.ac.jp/cgi-bin/cgi-asir-r-fd2.sh");
   print(ans1);
   ans2=
   curlPostMessage("oxMessageBody=fd_hessian2(-2,[-3,-2],10,[1/2,1/3])",
        "http://asir2.math.kobe-u.ac.jp/cgi-bin/cgi-asir-r-fd2.sh");
   ans2
)

-- --data-urlencode should be used for new curl.
curlPostMessage = method()
curlPostMessage(String,String) := (data,url) -> (
   curl("--data " | "'" | data | "'", url)
)

----------------------------------------------
-- Sample interface for cgi-asir.
---------------------------------------------
-- Translate list expressions among asir and M2
asir2m2 = method()
asir2m2(String):= (s) -> (
  s2=replace("\\[","{",s);
  replace("\\]","}",s2)
)
m22asir = method()
m22asir(String):= (s) -> (
  s2=replace("\\{","[",s);
  replace("\\}","]",s2)
)

-- Example: fdHessian2(-2,{-3,-2},10,{1/2,1/3})
fdHessian2 = method()
fdHessian2(ZZ,List,ZZ,List):= (a,b,c,x) -> (
 strategy = "asir2-cgi";
 url = "http://asir2.math.kobe-u.ac.jp/cgi-bin/cgi-asir-r-fd2.sh";
 if (strategy == "asir2-cgi") then (
  s = toString(a) | "," | m22asir(toString(b)) | "," |
      toString(c) | "," | m22asir(toString(x));
  s = "oxMessageBody=fd_hessian2(" | s | ")";
  ans = curlPostMessage(s,url);
  if (match("error",ans)) then error(ans);
  h=value(asir2m2(ans));
 );
 {h,ans}
)

-- s should be in the polynomial ring.
-- example: QQ[s,x,y,dx,dy,WeylAlgebra=>{x=>dx,y=>dy}]
--    genericBfct({x*dx+y*dy-3,x*dx-y*dy},{x,y},{dx,dy},{1,1})
genericBfct = method()
genericBfct(List,List,List,List):= (ideal,xvar,dvar,w) -> (
 strategy = "asir2-cgi";
 url = "http://asir2.math.kobe-u.ac.jp/cgi-bin/cgi-generic_bfct2.sh";
 if (strategy == "asir2-cgi") then (
  ss = m22asir(toString(ideal)) | "," | m22asir(toString(xvar)) | "," |
       m22asir(toString(dvar)) | "," | m22asir(toString(w));
  ss = "oxMessageBody=generic_bfct(" | ss | ")";
  ans = curlPostMessage(ss,url);
  if (match("error",ans)) then error(ans);
  h=value(asir2m2(ans));
 );
 {h,ans}
)

genericBfct(Ideal,List,List,List):= (ideal,xvar,dvar,w) -> (
  genericBfct(flatten(entries(gens(ideal))),xvar,dvar,w)
)
-- todo. Extract xvar,dvar. vars ring I


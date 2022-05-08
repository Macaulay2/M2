-- -*- coding: utf-8 -*-
newPackage(
        "OnlineLookup",
        Version => "0.7",
        Date => "March 23, 2022", -- "March 2, 2022",
        Authors => {{Name => "Paul Zinn-Justin",
                  Email => "pzinn@unimelb.edu.au",
                  HomePage => "http://blogs.unimelb.edu.au/paul-zinn-justin/"}},
        Headline => "Look up mathematical information online",
	Keywords => {"System"},
        DebuggingMode => false,
	AuxiliaryFiles => true,
	CacheExampleOutput => true,
	PackageImports => {"Text"}
        )

export {"oeis","urlEncode","isc"}

percentEncoding = hashTable transpose {
    {"␣", "!", "#", "$", "%", "&", "'", "(", ")", "*", "+", ",", "/", ":", ";", "=", "?", "@", "[", "]"},
    {"%20", "%21", "%23", "%24", "%25", "%26", "%27", "%28", "%29", "%2A", "%2B", "%2C", "%2F", "%3A", "%3B", "%3D", "%3F", "%40", "%5B", "%5D"}
    }

-- TODO: use in/move to html.m2
urlEncode = method()
urlEncode Nothing := identity
urlEncode String := s -> concatenate apply(s, c -> if percentEncoding#?c then percentEncoding#c else c)

oeisHTTP := "http://oeis.org";
oeisHTTPS := "https://oeis.org";

tryWWW = url -> try last splitWWW getWWW url else (
    << "--warning: failed to fetch from the web" << endl;
    return "";
    )


oeis = method(TypicalValue => OL,
    Options => {Limit => 100, Position => 0})
oeis VisibleList := o -> L -> oeis (demark(",",toString\L),o)
oeis String := o -> search -> (
    url:=oeisHTTP|"/search?q="|urlEncode search|"&fmt=text&start="|o.Position|"&n="|o.Limit; -- limit the number of results
    www :=  tryWWW url;
    ans := select("(?<=^%N ).*$",www);    
    OL apply(ans, line -> LI(
            blank := regex(" ",line);
            if blank === null then line -- shouldn't happen
            else (
                pos := blank#0#0;
                seq := substring(0,pos,line);
                {HREF {oeisHTTPS|"/"|seq,seq} ,substring(pos,line)}
            )))
    )
-- e.g. oeis {1,2,7,42}

isc = method(TypicalValue => OL)

binaryReplace = (bin,expr,sep,s) -> (
    while (i:=regex(bin,s)) =!= null do (
	i=i#0#0;
	j:=i-1; p:=0; while j>=0 and (p>0 or j==i-1 or (s#j!="," and s#j!="+" and s#j!="*" and s#j!="-")) do (if s#j=="(" then p=p-1 else if s#j==")" then p=p+1; if p<0 then break; j=j-1;);
	j=j+1;
	k:=i+1; p=0; while k<#s and (p>0 or k==i+1 or (s#k!="," and s#k!="+" and s#k!="*" and s#k!="-")) do (if s#k=="(" then p=p+1 else if s#k==")" then p=p-1; if p<0 then break; k=k+1;);
	s=substring(s,0,j) | expr | "(" | substring(s,j,i-j) | sep | substring(s,i+1,k-i-1) | ")" | substring(s,k);
	);
    s)

maple2M2 = s -> ( -- some common functions
    s = replace("^\\s+|\\s+$","",s);
    s = replace("sr","sqrt",s);
    s = replace("ln","log",s);
    s = replace("GAMMA|GAM","Gamma",s);
    s = replace("arc","a",s);
    s = replace("Pi","pi",s);
    s = replace("E\\^","exp",s);
    s = replace("E","(exp 1)",s);
    s = replace("gamma","EulerConstant",s);
    s = replace("Psi","Digamma",s);
    s = replace("Re","realPart",s);
    s = replace("Im","imaginaryPart",s);
    s = replace("I","ii",s);
    s = binaryReplace("\\^","",")`(",s); -- to fix precedence issue of ^ in maple
    s = replace("`","^",s); -- phew
    s
    )

expr = s -> (
    s = replace("([a-zA-Z]+)","\\(hold $1\\)",s);
    s = binaryReplace("\\^","Power",",",s);
    s = binaryReplace("/","Divide",",",s);
    s
    )

SPAN.Options = SPAN.Options ++ { "data-m2code" => null }

isc Number :=
isc Constant := isc @@ numeric
isc RR := x -> (
    s := format(0,-1,1000,1000,"",x);
    isc substring(s,0,#s-1) -- remove last digit for now... TODO better
    )
isc String := s -> (
    url := "http://wayback.cecm.sfu.ca/cgi-bin/isc/lookup?number="|urlEncode s|"&lookup_type=simple";
    www := tryWWW url;
    ans := select("(?<=<PRE>)[\\s\\S]*?(?=</PRE>)",www);
    if #ans == 0 then return {};
    ans = first ans;
    lst := apply(select(separate("\n\n",ans),x->#x>3 and substring(x,0,3)=="<B>"),x->replace("<.*?>","",x)); -- TODO limit number
    OL apply(lst, e -> (
	    y := separate("=",e);
	    if #y<=1 then return SPAN e;
	    c := maple2M2 last y;
	    cexpr := expr c;
	    try v:=value cexpr else return SPAN e;
	    SPAN ( drop(y,-1) | { "= ", SPAN { v, "data-m2code" => c } } )
	    ))
    )


beginDocumentation()
multidoc ///
 Node
  Key
   OnlineLookup
  Headline
   Look up mathematical information online
  Description
   Text
    The purpose of this package is to collect helper functions that allow to query web sites for mathematical
    information and format it into Macaulay2 output.
    At present, it contains two such functions, @TO{oeis}@ and @TO{isc}@, but more will be implemented in the future.
 Node
  Key
   oeis
   (oeis, String)
   (oeis, VisibleList)
   [oeis, Limit]
   [oeis, Position]
  Headline
   OEIS lookup
  Description
   Text
    This function looks up the argument (a list of integers or a string) in the Online Encyclopedia of Integer Sequences
    (@HREF "http://oeis.org/"@).
   Example
    oeis {1,3,31,1145}
   Text
    Optional arguments @TT"Limit"@ and @TT"Position"@ allow to control the maximum amount of listed answers.
   Example
    L = apply(5,n->n!);
    oeis (L,Limit=>5)
    oeis (L,Limit=>1,Position=>2)
 Node
  Key
   isc
   (isc, String)
   (isc, RR)
  Headline
   ISC lookup
  Description
   Text
    This function looks up the argument (a real number or a string) in the Inverse Symbolic Calculator.
    (@HREF "http://wayback.cecm.sfu.ca/projects/ISC/"@).
   Example
    isc (sqrt 2*pi)
 Node
  Key
   urlEncode
  Headline
   URL encoding
  Description
   Text
    This function provides a minimal encoding of a string in order to be used as part of a URL.
///

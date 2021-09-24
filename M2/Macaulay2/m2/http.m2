--		Copyright 1996 by Daniel R. Grayson

needs "methods.m2"

getWWW = method()
httpProduct := concatenate("Macaulay2/", version#"VERSION")

GET := (host,url,connection) -> (
     connection = 
     openInOut connection
     << "GET " << url << " HTTP/1.1" << endl
     << "User-Agent: " << httpProduct << endl
     << "Connection: close" << endl
     << "Host: " << host << endl
     << endl << flush;
     first( get connection, close connection )
     )

POST := (host,url,body,connection) -> (
     connection = 
     openInOut connection
     << "POST " << url << " HTTP/1.1" << endl
     << "User-Agent: " << httpProduct << endl
     << "Connection: close" << endl
     << "Host: " << host << endl
     << "Content-type: application/x-www-form-urlencoded" << endl
     << "Content-length: " << # body << endl << endl
     << body << endl
     << endl << flush;
     first( get connection, close connection )
     )

getpost := (host,url,body,connection) -> (
     if body === null
     then GET(host,url,connection)
     else POST(host,url,body,connection)
     )

protocols := {
     ("http://" , (host,port,url,body) -> getpost(host, url, body, (
		    "$" | host | ":" | (if port =!= null then port else "http")
		    ))),
     ("https://", (host,port,url,body) -> (
	       cmd := (
	       	    "!openssl s_client -quiet -verify 1 " |
	       	    " -host " | host | " -port " | (if port =!= null then port else "443")
		    | " 2>/dev/null"
	       	    );
	       if debugLevel > 0 then << "cmd: " << cmd << endl;
	       getpost(host, url, body, cmd)))
     }

getWWW String := url -> getWWW(url,)

getWWW (String,String) :=
getWWW (String,Nothing) := (url,body) -> (
     ret := "";
     select(1,protocols,(prot,meth) -> (
     	       if substring(0,#prot,url) == prot 
	       then (
		    url = substring(#prot,url);
		    host := (separate("/",url))#0;
		    url = substring(#host,url);
		    if url == "" then url = "/";
		    port := null;
		    x := separate(":",host);
		    if #x == 2 then (host = x#0; port = x#1);
		    ret = meth(host,port,url,body);
		    true)
	       else false));
     ret)

hexdigits = new HashTable from {
    "0" => 0,
    "1" => 1,
    "2" => 2,
    "3" => 3,
    "4" => 4,
    "5" => 5,
    "6" => 6,
    "7" => 7,
    "8" => 8,
    "9" => 9,
    "a" => 10,
    "b" => 11,
    "c" => 12,
    "d" => 13,
    "e" => 14,
    "f" => 15,
    "A" => 10,
    "B" => 11,
    "C" => 12,
    "D" => 13,
    "E" => 14,
    "F" => 15
    }
hex = (str) -> (
    -- str: String, coming from getWWW
    -- each char of str should be 0..9,a..f, or A..F
    -- transform this string into the corresponding hexadecimal number
    result := 0;
    for c in characters str do result = 16*result + hexdigits#c;
    result
    )
header = (str) -> (
    -- str: String, coming from getWWW
    -- return ((offset,len of header), (offset, len of body))
    loc := regex("\r\n\r\n", str);
    if loc === null then error "string not in http response format (expected \\r\\n\\r\\n to appear in the string, signalling the end oof the header";
    ((0,loc#0#0), (plus loc#0, #str - plus loc#0))
    )
extractChunk = (offset,str) -> (
    -- offset: offset of the start of the chunk in str
    -- str: String, coming from getWWW
    --
    -- format of a chunk
    -- hexadecimal digits giving the length n
    -- \r\n
    -- n characters
    -- \r\n
    --
    -- return value: (newoffset, thischunk)
    loc := regex("([0-9a-fA-F]+)\r\n", offset, str);
    if loc#0#0 =!= offset then error "chunk format error";
    len := hex substring(loc#1, str);
    chunkstart := plus loc#0;
    thischunk := substring(chunkstart, len, str);
    (chunkstart+len+2, thischunk)               -- +2 passes over \r\n
    )

unchunk = (offset,str) -> (
    -- offset: offset of the start of the chunk in str
    -- str: String, coming from getWWW
    concatenate while offset < #str list (
        (newoffset, chunk) := extractChunk(offset,str);
        offset = newoffset;
        chunk
        )
    )

splitWWW = method()
splitWWW String := (str) -> (
    (head,rest) := header str;
    chunked := regex("Transfer-Encoding: chunked", head#0, head#1, str);
    body := if chunked === null then substring(rest, str)
      else unchunk(rest#0, str);
    (substring(head,str),body)
    )

httpHeaders = method()
httpHeaders String := s -> concatenate(
-- for documentation of http protocol see http://www.w3.org/Protocols/rfc2616/rfc2616.html
"HTTP/1.1 200 OK
Server: ", httpProduct, "
Connection: close
Content-Length: ", toString length s, "
Content-type: text/html; charset=utf-8

", s)

-----------------------------------------------------------------------------

-- test POST method like this:

--    i1 : getWWW("http://qs.secapl.com/cgi-bin/qs","tick=VTSMX")
--    
--    o1 = HTTP/1.0 200 OK
--         Server: Netscape-Commerce/1.1
--         Date: Friday, 24-Dec-99 23:15:06 GMT
--         Content-type: text/html
--         
--         <html><head>
--         <title>CheckFree Investment Services Quote Server</title>
--         </head>
--         <body background="http://www.secapl.com/images/bkgrnd_tile.gif">
--     	       ...
--         <hr width=200 size=3 noshade>
--         <center><font face="arial,helvetica" size="-1">
--         <i><a href=http://www.secapl.com/HOMELink>CheckFree Investment Services</a><br>
--         <a href="mailto:g.apl@secapl.com">g.apl@secapl.com</a></i>
--         </font></center></body></html>
--         
--    
--    o1 : String

-----------------------------------------------------------------------------

--  
--  
--  Here is the way netscape queries us:
--  
--  o7 = GET / HTTP/1.0
--       Connection: Keep-Alive
--       User-Agent: Mozilla/4.05 [en] (X11; U; Linux 2.1.121 i586; Nav)
--       Host: localhost:2500
--       Accept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, image/png, */*
--       Accept-Language: en-US,en,en-GB,de,fr,fr-FR,ru,ja
--       Accept-Charset: iso-8859-1,*,utf-8
--  
--  Here is the way wget queries us:
--  
--  o5 = HEAD / HTTP/1.0
--       User-Agent: Wget/1.5.1
--       Host: localhost:2500
--       Accept: */*
--  
--  Here is the way Macaulay2 does it.
--  
--  o12 = GET / HTTP/1.0
--        User-Agent: Macaulay2
--  
--  

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:

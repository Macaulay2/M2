--		Copyright 1996 by Daniel R. Grayson

getWWW = method()

getWWW String := memoize ((url) -> (
	  if substring(url,0,7) == "http://" then (
	       url = substring(url,7);
	       host := (lines(url,"/"))#0;
	       url = substring(url, # host);
	       if url == "" then url = "/";
	       f := openInOut( "$" | host | ":80" );
	       f << "GET " << url << " HTTP/1.0" << endl;
	       f << "User-Agent: Macaulay2" << endl;
	       f << endl << flush;
	       s := get f;
	       close f;
	       s)
	  else ""
	  ))

getWWW(String, String) := memoize (
     (url,content) -> (
	  if substring(url,0,7) == "http://" then (
	       url = substring(url,7);
	       host := (lines(url,"/"))#0;
	       url = substring(url, # host);
	       if url == "" then url = "/";
	       f := openInOut( "$" | host | ":80" );
	       f << "POST " << url << " HTTP/1.0" << endl
	       << "User-Agent: Macaulay2" << endl
	       << "Content-type: application/x-www-form-urlencoded" << endl
	       << "Content-length: " << # content << endl << endl
	       << content << endl
	       << endl << flush;
	       s := get f;
	       close f;
	       s)
	  else ""
	  )
     )

document { quote getWWW,
     TT "getWWW URL", " -- obtain the contents of a URL from an http server.",
     BR,NOINDENT,
     TT "getWWW(URL,TEXT)", " -- obtain the contents of a URL from an 
     http server, using the 'POST' method, provided with the TEXT.",
     PARA,
     "This doesn't work under solaris because Sun doesn't provide sockets
     or name service to statically linked programs like this one."
     }

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
--  Here is the way Macaulay 2 does it.
--  
--  o12 = GET / HTTP/1.0
--        User-Agent: Macaulay2
--  
--  

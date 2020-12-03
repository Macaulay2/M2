use varstrin;
use arithmetic;

export tostring(i:long):string := (
     if i == long(0) then return "0";
     s := newvarstring(25);
     signl := i<0;
     if signl then i=-i;
     while i>0 do (
	  s << "0123456789".(i%10);
	  i = i/10;
	  );
     toreversestring(if signl then s << '-' else s));
export tostring(i:ulong):string := (
     if i == ulong(0) then return "0";
     s := newvarstring(25);
     while i>0 do (
	  s << "0123456789".(i%10);
	  i = i/10;
	  );
     toreversestring(s));

export tostring(i:int):string := (
     if i==0 then return "0";
     s := newvarstring(25);
     signl := i<0;
     if signl then i=-i;
     while i>0 do (
	  s << "0123456789".(i%10);
	  i = i/10;
	  );
     toreversestring(if signl then s << '-' else s));


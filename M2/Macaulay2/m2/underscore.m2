--		Copyright 1995 by Daniel R. Grayson

Sequence _ ZZ := List _ ZZ := (s,i) -> s#i

String _ ZZ := String => (s,i) -> s#i
String _ Sequence := String => (s,i) -> ((j,k) -> substring(j,k,s)) i




s = (x,y) -> if y =!= 0 then (
     --<< "x = " << x << endl;
     --<< "y = " << y << endl;
     q := x // y;
     r := x % y;
     if not ( q*y + r == x and 0 <= r and r < abs y )
     then (
	  stderr << "--------------------------------------------" << endl;
	  stderr << "x = " << x << endl;
	  stderr << "y = " << y << endl;
	  stderr << "q = x//y = " << q << endl;
	  stderr << "r = x%y  = " << r << endl;
	  stderr << "q*y+r  = " << q*y+r << endl;
	  );
     assert( q*y + r == x );
     assert( 0 <= r );
     assert( r < abs y );
     )
t = (m,n) -> (
     s(m,n);
     s(m*n+1,n);
     s(m*n-1,n);
     )
u = (m,n) -> (
     t(m,n);
     t(m,-n);
     t(-m,n);
     t(-m,-n);
     )
r = (m,n) -> (
     u(m-1,n);
     u(m,n);
     u(m+1,n);
     )
v = (m,n) -> (
     r(m,n-1);
     r(m,n);
     r(m,n+1);
     )
w = (m,n) -> (
     v(m,n);
     v(n,m);
     )
x = (m,n) -> (
     w(m,n);
     w(2*m,n);
     w(m,2*n);
     w(2*m,2*n);
     )     
y = (m,n) -> (
     x(m,n);
     x(m*m,n);
     x(m,n*n);
     x(m*m,n*n);
     )
y(0,123)
y(0,123456)
y(0,12372034958723094857)
y(0, 2^15)
y(0, 2^31)
y(0, 2^63)
y(12,340)
y(12,234523423984289734)
y(123572093845,23348572034957820348523)
y(123572093845,2334857203495782034852339845739485)
y(5239405728934572435, 2^15)
y(5239405728934572435, 2^31)
y(5239405728934572435, 2^63)
y(1234567, 2^15)
y(1234567, 2^31)
y(1234567, 2^63)
y(123, 2^15)
y(123, 2^31)
y(123, 2^63)

assert( powermod(5,(3331333-1)*20000,3331333) == 1 )

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test integer.out"
-- End:

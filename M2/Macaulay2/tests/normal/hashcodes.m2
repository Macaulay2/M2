-- these tests ensure that strings are arrays of signed characters and that hash codes are positive

     assert( ( hash ascii { 1 } ) === 1 );
     assert( ( hash ascii { -1 } ) === 2147483647 );
     assert( ( hash ascii { 1,2 } ) === 33 );
     assert( ( hash ascii { 1,-2 } ) === 29 );
     assert( ( hash ascii { -1,-2 } ) === 2147483615 );


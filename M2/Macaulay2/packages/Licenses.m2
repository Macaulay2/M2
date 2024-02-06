newPackage ( "Licenses",
     Version => "1", 
     Date => "February, 2008",
     Authors => {{Name => "Daniel R. Grayson", 
	       Email => "danielrichardgrayson@gmail.com", 
	       HomePage => "http://dangrayson.com/"}},
     Keywords => {"Miscellaneous"},
     Headline => "licensing of Macaulay2"
     )

export "checkLicense"

-- We issue the M2 binary under GPL 3, and libraries licensed under the following licenses
-- can be linked with our binary:
licenses = set {
     "GPL 2",
     "GPL 2.1",
     "GPL 3",
     "LGPL 2",
     "LGPL 2.1",
     "LGPL 3",
     "GPL 2 or 3",
     "GPL 2 or later",
     "GPL 2.1 or later",
     "GPL 3 or later",
     "LGPL 2 or later",
     "LGPL 2.1 or later",
     "LGPL 3 or later",
     "CeCILL-B",
     "Boost",
     "MIT",
     "public domain",
     "modified BSD",
     "free"	-- copyright, but freely distributable with copyright preserved
     }

licenseInfo = hashTable {
    "atomic_ops version" => hashTable {
	 "7.4.6" => VerticalList {
	      "MIT",
	      "Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the \"Software\"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:  The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software."
	      }
         },
    "factory version" => hashTable {
	 "4.1.0" => VerticalList {
	      "GPL 2 or 3",
	      "This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation ( version 2 or version 3 of the License)"
	      }
         },
    "fflas_ffpack version" => hashTable {
	 "2.3.0" => VerticalList {
	      "LGPL 2.1 or later",
	      "FFLAS-FFPACK is free software: you can redistribute it and/or modify it under the terms of the  GNU Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option) any later version."
	      }
         },
    "flint version" => hashTable {
	 "2.5.2" => VerticalList {
	      "GPL 2 or later",
	      "FLINT is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version."
	      }
         },
    "frobby version" => hashTable {
    	 "0.9.0" => VerticalList {
	      "GPL 2 or later",
	      "This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version."
	      }
         },
    "gc version" => hashTable {
	 "7.6.0" => VerticalList {
	      "free",
	      "Permission is hereby granted to use or copy this program for any purpose,  provided the above notices are retained on all copies. Permission to modify the code and to distribute modified code is granted, provided the above notices are retained, and a notice that the code was modified is included with the above copyright notice."
	      }
        },
    "givaro version" => hashTable {
	 "4.0.2" => VerticalList {
	      "CeCILL-B",
	      "Givaro is governed by the CeCILL-B license under French law and abiding by the rules of distribution of free software."
	      }
        },
    "gmp version" => hashTable {
        },
    "linbox version" => hashTable {
        },
    "mathic version" => hashTable {
	 "1.0" => VerticalList {
	      "LGPL 2 or later",
	      "Mathic is licensed for use under the terms of GNU Lesser General Public License version 2.0, and under any later version; the option is yours."
	      }
        },
    "mathicgb version" => hashTable {
	 "1.0" => VerticalList {
	      "GPL 2 or later",
	      "Mathicgb is licensed for use under the terms of GNU General Public License version 2 and under any later version; the option is yours."
	      }
        },
    "memtailor version" => hashTable {
	 "1.0" => VerticalList {
	      "modified BSD",
	      "MemTailor is distributed under the Modified BSD License."
	      }
        },
    "mpfr version" => hashTable {
	 "3.1.4" => VerticalList {
	      "LGPL 3 or later",
	      "The GNU MPFR Library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 3 of the License, or (at your option) any later version."
	      }
        },
    "mpir version" => hashTable {
	 "2.7.2" => VerticalList {
	      "free",
	      "Copying and distribution of this file, with or without modification, are permitted in any medium without royalty provided the copyright notice and this notice are preserved."
	      },
	 "3.0.0" => VerticalList {
	      "LGPL 2.1 or later",
	      "The MPIR Library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option) any later version."
	      }
        },
    "mpsolve version" => hashTable {
        "3.1.8" => VerticalList {
              "GPL 3",
              "License: http://www.gnu.org/licenses/gpl.html GPL version 3 or higher"
            }
        },
    "boost version" => hashTable {
        "1.69" => VerticalList {
              "Boost",
              "License: https://www.boost.org/users/license.html Boost Software License"
            }
        },
    "mysql version" => hashTable {
        },
    "ntl version" => hashTable {
	 "10.3.0" => VerticalList {
	      "LGPL 2.1 or later",
	      "NTL is open-source software distributed under the terms of the GNU Lesser General Public License (LGPL) version 2.1 or later."
	      }
        },
    "python version" => hashTable {
        },
    "readline version" => hashTable {
	 "6.3" => VerticalList {
	      "GPL 3 or later",
	      "The Readline library is free software, distributed under the terms of the [GNU] General Public License as published by the Free Software Foundation, version 3 of the License."
	      }
        },
    "scscp version" => hashTable {
        }
   }

for lib in keys licenseInfo do
for ver in keys licenseInfo#lib do
	  if not licenses#?(first licenseInfo#lib#ver)
	  then error ("invalid license: ", first licenseInfo#lib#ver)

lic = (lib,ver) -> if licenseInfo#?lib and licenseInfo#lib#?ver then licenseInfo#lib#ver

reload = new Command from (() -> loadPackage ("License", Reload => true))

checkLicense = () -> (
     libraries := sort select (keys version, k -> match (" version$", k));
     libraries = select(libraries, lib -> version#lib =!= "not present");
     hashTable for lib in libraries list (
	  ver := version#lib;
	  (concatenate (lib, " ", ver)) => lic (lib, ver)))

beginDocumentation()

multidoc ///
Node
 Key
   Licenses
 Description
   Text 
       This package examines the version number of the various packages compiled
       with Macaulay2 to determine under which licenses Macaulay2 may be offered.
///

-- this test doesn't ever produce an error, and thus the warning is invisible, since it 
-- normally redirected to a file
TEST ///
    print checkLicense()
    if member (null, values checkLicense())
    then (
	 stderr << "Licenses: *** Warning: unknown license for some packages:" << endl
	        << VerticalList keys select (checkLicense(), ver -> ver === null) << endl
	 )
///

newPackage ( "License",
     Version => "1", 
     Date => "February, 2008",
     Authors => {{Name => "Daniel R. Grayson", 
	       Email => "danielrichardgrayson@gmail.com", 
	       HomePage => "http://dangrayson.com/"}},
     Headline => "licensing of Macaulay2",
     DebuggingMode => true
     )

export "checkLicense"

needsPackage "SimpleDoc"
needsPackage "Text"

multidoc ///
Node
 Key
   License
 Description
   Text 
       This package examines the version number of the various packages compiled
       with Macaulay2 to determine under which licenses Macaulay2 may be offered.
///

TEST ///
  checkLicense()
  ///

licenses = set {
     "GPL 2",
     "GPL 2.1",
     "GPL 3",
     "LGPL 2",
     "LGPL 2.1",
     "LGPL 3",
     "GPL 2 or later",
     "GPL 2.1 or later",
     "GPL 3 or later",
     "LGPL 2 or later",
     "LGPL 2.1 or later",
     "LGPL 3 or later",
     "public domain",
     "free"	-- copyright, but freely distributable with copyright preserved
     }

licenseInfo = hashTable {
    "atomic_ops version" => hashTable {
        },
    "factory version" => hashTable {
        },
    "fflas_ffpack version" => hashTable {
        },
    "flint version" => hashTable {
        },
    "frobby version" => hashTable {
        },
    "gc version" => hashTable {
        },
    "givaro version" => hashTable {
        },
    "gmp version" => hashTable {
        },
    "linbox version" => hashTable {
        },
    "mathic version" => hashTable {
        },
    "mathicgb version" => hashTable {
        },
    "memtailor version" => hashTable {
        },
    "mpfr version" => hashTable {
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
    "mysql version" => hashTable {
        },
    "ntl version" => hashTable {
        },
    "pari version" => hashTable {
	 "2.9.2" => VerticalList {
	      "GPL 2 or later",
	      "PARI/GP is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version."     }
        },
    "python version" => hashTable {
        },
    "readline version" => hashTable {
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

print checkLicense()
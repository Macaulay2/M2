/^#define MP_Cop/ {
	s/0x//
	s/#define MP_\([^ 	]*\)[ 	][ 	]*\([0-9][0-9]*\)/MP\1 = \2; protect quote \1;/
	s/_//g
	s=/\*.*\*/==
	bokay
	}
/^#define MP_Cmt/ {
	s/0x//
	s/#define MP_\(.*\)[ 	]\([0-9][0-9]*\)/MP\1 = \2; protect quote \1;/
	s/_//g
	s=/\*.*\*/==
	bokay
	}
/^#define MP_Annot/ {
	s/0x//
	s/#define MP_\(.*\)[ 	]\([0-9][0-9]*\)/MP\1 = \2; protect quote \1;/
	s/_//g
	s=/\*.*\*/==
	bokay
	}
/^#define MP_Cc/ {
	s/0x//
	s/#define MP_\(.*\)[ 	]\([0-9][0-9]*\)/MP\1 = \2; protect quote \1;/
	s/_//g
	s=/\*.*\*/==
	bokay
	}
/^#define MP_.*Dict[ 	]/ {
	s/0x//
	s/#define MP_\(.*\)[ 	]\([0-9][0-9]*\)/MP\1 = \2; protect quote \1;/
	s/_//g
	s=/\*.*\*/==
	bokay
	}
d
:okay

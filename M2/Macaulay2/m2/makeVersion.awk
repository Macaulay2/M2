BEGIN {
     print "version = hashTable {";
     printf("       (\"ARCH\" , \"%s\"),\n", ENVIRON["ARCH"]);
     printf("       (\"OS\" , \"%s\"),\n", ENVIRON["OS"]);
     printf("       (\"REL\" , \"%s\"),\n", ENVIRON["REL"]);
     printf("       (\"VERSION\" , \"%s\"),\n", ENVIRON["VERSION"]);
     printf("       (\"VERSIONDATE\" , \"%s\"),\n", ENVIRON["VERSIONDATE"]);
     printf("       (\"COMPILETIME\" , \"%s\")\n", strftime());
     print  "       }";
     }

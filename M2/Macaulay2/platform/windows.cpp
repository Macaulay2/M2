
//this code is from Macaulay2_main and performs path mangling.
//This is going to have to be put back in for the windows build.
#if defined(_WIN32)
     if (argv[0][0]=='/' && argv[0][1]=='/' && argv[0][3]=='/') {
       /* we must be in Windows 95 or NT running under CYGWIN32, and
	  the path to our executable has been mangled from D:/a/b/c
	  into //D/a/b/c */
       argv[0][0] = argv[0][2];
       argv[0][1] = ':';
       strcpy(argv[0]+2,argv[0]+3);
     }
     {
       /* change all \ in path to executable to / */
       char *p;
       for (p=argv[0]; *p; p++) if (*p == '\\') *p = '/';
     }
#endif

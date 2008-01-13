p = getWWW "http://www.math.uiuc.edu/Macaulay2/Makefile"
r = regex("\r\n\r\n",p)
substring(p,r#0#0+4)
peek oo
substring(p,0,r#0#0+2)
peek oo

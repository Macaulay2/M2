
# for windows nt nmake

all : index.html

index.html : ..\m2\*.m2
	del *.html
	..\bin\M2 html.m2 -eexit(0)

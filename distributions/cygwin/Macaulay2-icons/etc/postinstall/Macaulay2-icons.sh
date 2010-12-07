#! /bin/sh -e
LOGFILE=/var/log/Macaulay2-icons.log
echo logging stderr to $LOGFILE >&2
exec 2>$LOGFILE
date
set -x
for destination in DESKTOP STARTMENU
do case $destination in
       DESKTOP)
	   OPTS="--allusers --desktop"
	   SUBDIR=.
	   ;;
       STARTMENU)
	   OPTS="--allusers --smprograms" 
	   SUBDIR=Macaulay2
	   ;;
   esac
   destdir=`cygpath --unix $OPTS`
   [ -d "$destdir"/$SUBDIR ] || mkdir "$destdir"/$SUBDIR
   mkshortcut $OPTS \
	--icon="/bin/XWin.exe" \
	--desc="use this to start Macaulay2 if X windows is not yet running" \
	--name="$SUBDIR/XWin + emacs + M2" \
	--workingdir="/bin" \
	/bin/run.exe \
	--arguments="/usr/bin/bash.exe -l -c \"/usr/bin/startxwin.exe /usr/bin/emacs -l M2-init.el -f M2\""
   for display in 0
   do mkshortcut $OPTS \
	--icon="/bin/emacs.ico" \
	--desc="use this to start Macaulay2 if X windows is running on display $display" \
	--name="$SUBDIR/emacs + M2 on $display" \
	--workingdir="/bin" \
	/bin/run.exe \
	--arguments="/usr/bin/bash.exe -l -c \"/usr/bin/emacs-X11 --display :$display -l M2-init.el -f M2\""
   done
done

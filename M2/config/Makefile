# the files config.guess and config.sub used be at /ftp@ftp.gnu.org:/gnu/config/, but now the
# README file there tells where to get them instead

guess:
	./config.guess
update:
	rm -f config.guess* config.sub*
	wget -O config.guess "http://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=config.guess;hb=HEAD"
	wget -O config.sub   "http://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=config.sub;hb=HEAD"
	chmod a+x config.sub config.guess

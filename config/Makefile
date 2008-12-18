# /ftp@ftp.gnu.org:/gnu/config/

update-files:
	rm -f config.guess* config.sub*
	wget http://savannah.gnu.org/cgi-bin/viewcvs/*checkout*/config/config/config.guess
	wget http://savannah.gnu.org/cgi-bin/viewcvs/*checkout*/config/config/config.sub
	chmod a+x config.sub config.guess

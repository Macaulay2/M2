pre-install install all reconfigure reconfigure-all status distclean clean::
	for i in * */* ; do if [ -f "$$i"/GNUmakefile ] ; then $(MAKE) -C "$$i" $@ ; fi ; done
# distclean clean::; rm -rf CommonStagingArea/*

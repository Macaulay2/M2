all reconfigure reconfigure-all status :
	@ for i in * ; do if [ -d $$i ] ; then (set -x ; $(MAKE) -C $$i "$@") || exit 1 ; fi ; done

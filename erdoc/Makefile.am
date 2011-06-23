dist_bin_SCRIPTS =	\
	erdoc.pl	\
	erext.pl	\
	ermerge.pl

erdocdir = $(datadir)/erdoc
dist_erdoc_DATA =	\
	erdoc.xsl	\
	erdoc.css	\
	merge.xsl

erdoc.sh: erdoc.sh.in
	cat erdoc.sh.in | sed "s*@erdoc_data@*${datarootdir}/erdoc*" > erdoc.sh
	chmod a+x erdoc.sh

all-local: erdoc.sh

erdoc_DATA = erdoc.sh
EXTRA_DIST = erdoc.sh.in


#!/bin/bash

DOC_DIR=
#DOC_DIR=mync

FILES="../*.c ../*.cpp ../*.h"

mkdir erdoc
mkdir erdoc/html

cd erdoc

if (( $? )); then exit -1; fi

{
    for FILE in $FILES; do
	echo "/** .open */"
	cat $FILE
	echo "/** .close */ /** .eof */"
    done
} | erext.pl | erdoc.pl cat $DOC_DIR > types.xml

{
    for FILE in $FILES; do
	echo "/** .open */"
	cat $FILE
	echo "/** .close */ /** .eof */"
    done
} | erext.pl | erdoc.pl sp $DOC_DIR > local_types.xml

ermerge.pl \
	local_types.xml \
	> types.xml

#ermerge.pl \
#	local_types.xml \
#	../../../mycpp/mycpp/erdoc/local_types.xml \
#	> types.xml

{
    for FILE in $FILES; do
	echo "/** .open */"
	cat $FILE
	echo "/** .close */ /** .eof */"
    done
} | erext.pl | erdoc.pl > out.xml

cd html

xsltproc /usr/local/share/erdoc/erdoc.xsl ../out.xml > index.html
cp /usr/local/share/erdoc/erdoc.css ./


cd ../..


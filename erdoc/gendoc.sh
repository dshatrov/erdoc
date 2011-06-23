#!/bin/bash

#DOC_DIR=html
DOC_DIR=erdoc

#FILES="../*.c ../*.cpp ../*.h"
FILES="../erdoc_doc.h"

mkdir erdoc
mkdir erdoc/$DOC_DIR

cd erdoc

if (( $? )); then exit -1; fi

{
    for FILE in $FILES; do
	echo "/** .open */"
	cat $FILE
	echo "/** .close */ /** .eof */"
    done
} | ../erext.pl | ../erdoc.pl cat $DOC_DIR > types.xml
#} | ../erext.pl ;exit; #| ../erdoc.pl cat $DOC_DIR > types.xml

{
    for FILE in $FILES; do
	echo "/** .open */"
	cat $FILE
	echo "/** .close */ /** .eof */"
    done
} | ../erext.pl | ../erdoc.pl sp $DOC_DIR > local_types.xml

#exit

../ermerge.pl local_types.xml > types.xml

#../ermerge.pl \
#	local_types.xml \
#	../../../mycpp/mycpp/erdoc/local_types.xml \
#	> types.xml

{
    for FILE in $FILES; do
	echo "/** .open */"
	cat $FILE
	echo "/** .close */ /** .eof */"
    done
} | ../erext.pl | ../erdoc.pl > out.xml

cd $DOC_DIR

xsltproc ../../erdoc.xsl ../out.xml > index.html
#cp /usr/local/share/erdoc/erdoc.css ./
cp ../../erdoc.css ./

cd ../..


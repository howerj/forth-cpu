#!/bin/bash

FILE_NAME=template
PDF_READER=evince

if
	pdflatex -halt-on-error $FILE_NAME.tex && pdflatex $FILE_NAME.tex &> /dev/null
then
	$PDF_READER $FILE_NAME.pdf &> /dev/null;
	COUNT=$(untex $FILE_NAME.tex | wc | awk '{print $2}')
	echo "Final word count:$COUNT";
	echo "Cleaning up!";
	rm -v *.aux *.log;
else
	echo "Failure!"
fi

#!/bin/bash

FILE_NAME=thesis
PDF_READER=evince

if
	pdflatex -halt-on-error $FILE_NAME.tex && pdflatex $FILE_NAME.tex &> /dev/null
then
	$PDF_READER $FILE_NAME.pdf &> /dev/null;
  echo "For $FILE_NAME.tex:";
  ./wc.tex.sh $FILE_NAME;
	echo "Cleaning up!";
else
	echo "Failure!"
fi

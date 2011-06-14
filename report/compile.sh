#!/bin/bash
if [ ! $1 ]
then
    filename="main"
else
    filename=$1
fi

rubber --pdf --force ${filename}

# pdflatex $filename | grep "arning"
# bibtex $filename | grep "arning"
# pdflatex $filename | grep "arning"
# pdflatex $filename | grep "arning"
# dvipdf $filename

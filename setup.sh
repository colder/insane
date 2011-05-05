#!/bin/bash

if [ $# -lt 1 ]; then
    echo "usage: $0 <env|config>"
    exit 1;
fi

abspath=$(cd ${0%/*} && echo $PWD/${0##*/})
from=`dirname $abspath`

echo "Importing $1..."

for f in `ls -A1 $from/$1`; do
    fil=`basename $f`
    ln -sf $from/$1/$f ~/$fil
done

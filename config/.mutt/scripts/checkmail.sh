#!/bin/sh
#if [ "$1" = "-v" ]; then
#    fetchmail -v
#else
#    fetchmail >/dev/null 2>&1
#fi

n=`find ~/Mail/ -path "*new/*" | grep -v Duplicates | wc -l`

if [ "$n" -gt 0 ]; then
	title="$n new mails"

	rm -rf transcript

    php ~/.mutt/scripts/check-folders.php > transcript
    
#	for f in `find ~/Mail/ -path "*new/*" | grep -v Duplicates`; do
#		dir=`echo $f | sed -e 's/.*\/Mail\///' | sed -e 's/\/new.*//'`
#		from=`cat $f | formail -x "From:"`
#		subj=`cat $f | formail -x "Subject: "`
#
#		echo "[$dir] $from" >> transcript
#		echo "$subj" >> transcript
#		echo  >> transcript
#	done

	nw=`wc -w transcript`

	notify-send -t $((1000+300*`echo $nw`)) -u low -i gtk-dialog-info "$title"
fi


#php ~/.mutt/scripts/check-folders.php

#!/bin/sh
printf "\033]0;$2\007" > /dev/tty
echo "$1"

#!/bin/bash

s=0
for i in *.pas ; do
    s=$[$s + `cat $i | wc -l`]
done
for i in *.fph ; do
    s=$[$s + `cat $i | wc -l`]
done
s=$[$s + `cat rpn.lpr | wc -l`]
echo $s
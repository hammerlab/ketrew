#!/usr/bin/env bash

CURRENT=`ls bisect*.out`
#echo current $CURRENT
eval $@
NOW=`ls bisect*.out`
#echo now $NOW

NEW=""
for f in $NOW; do
  echo $CURRENT | grep $f
  if [[ "$?" -ne "0" ]]; then
    NEW=$NEW" "$f
  fi
done

#echo new $NEW

DIRNAME="_report_dir"$(date +%Y%m%d_%H%M%S)
#echo $DIRNAME
mkdir $DIRNAME
bisect-report -I _build -html $DIRNAME $NEW
rm -rf $NEW

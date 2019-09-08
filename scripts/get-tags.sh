#!/bin/bash

DIR=$1

[ -z $DIR ] && echo "Specify the journal directory. Usage: get-tags.sh ~/Documents/journal/" && exit -1

get-col () {
	  if [ -z $1 ]
	  then
		    echo "Usage: $0 <column-number> [<delimeter>]"
		    return -1
	  fi
	  local colNum=$1
	  local delim=${2:- }
	  local cmd="awk -F '"$delim"' '{print $"$colNum"}' "
	  eval $cmd
}

cd $DIR

ls -ltra | get-col 9 | grep -e "[0-9]$" | xargs cat | grep '| \\_  ' | awk -F '|' '{print tolower($2)}' | get-col 2 : | sed -E s/'[0-9]{2} '//g | sed -E s/' +$'//g | sort | uniq -c | sort -n

cd - > /dev/null

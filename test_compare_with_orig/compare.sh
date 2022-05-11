#!/bin/bash

exec 3< <(find ./output/ -name "*.txt" | sort )

while read -u 3 -r file; do
	basename="${file##*\/}";
	while :; do
		set -x
		if cmp "$file" "./output_orig/$basename"; then
			{ set +x; } 2>/dev/null
			break
		else
			{ set +x; } 2>/dev/null
			read -r -p "[v]imdiff/[d]iff/[n]ext/[q]uit: " response
			case $response in
				[Vv]* ) vimdiff "$file" "./output_orig/$basename";;
				[Dd]* ) diff "$file" "./output_orig/$basename";;
				[SsNn]* ) break;;
				[Qq]* ) exit;;
				* ) echo "Please answer correctly.";;
			esac
		fi
	done
done

#!/bin/bash -x

ls -1 */main.go |
	while read file; do
		dir="${file%%\/*}";
		cd "$dir";
		~/go/bin/migoinfer main.go > main.migo
		cd ..;
	done

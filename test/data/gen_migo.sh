#!/bin/bash -xe


# https://stackoverflow.com/a/246128
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

list_files() {
        if [ $# -gt 0 ]; then
		find . -name "$1" | sort
        else
                echo $@
        fi
}


BASE_PATH="$PWD"

list_files "main.go" $@ |
	while read FILE; do
		echo $FILE
		DIR="${FILE%\/*}";
		cd "$DIR";

		migoinfer main.go > main.migo;
		cd "$BASE_PATH"
	done

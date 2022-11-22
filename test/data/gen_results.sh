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

		if ! [ -e "main.migo" ]; then
			~/go/bin/migoinfer main.go > main.migo;
		fi
		cd "$BASE_PATH"
	done


list_files "*.migo" $@ |
	while read FILE; do
		echo $FILE
		DIR="${FILE%\/*}";
		BASENAME="$(basename ${FILE})"
		FILENAME="${BASENAME%.*}"
		cd "$DIR";
		docker run --rm -v "$PWD":/root/go nickng/gong:popl17ae /bin/bash -c "time Gong -A /root/go/$FILENAME.migo" |& tee "$FILENAME".gong || true
		docker run -t --rm -v $(pwd):/root jgabet/godel2:latest /bin/sh -c "Godel -T $FILENAME.migo; Godel $FILENAME.migo" |& tee $FILENAME.godel2 || true
		dune exec dlock -- -ds 1 -s -m "$FILENAME".migo &> "$FILENAME".dlock.ds1 || true
		timeout 10 dune exec dlock -- -ds 2 -s -m "$FILENAME".migo &> "$FILENAME".dlock.ds2 || true
		dune exec dlock-orig -- -ds 1 -s -m "$FILENAME".migo &> "$FILENAME".dlock-orig.ds1 || true
		dune exec dlock-orig -- -ds 2 -s -m "$FILENAME".migo &> "$FILENAME".dlock-orig.ds2 || true
		cd "$BASE_PATH"
	done

list_files "*.ccs" $@ |
	while read FILE; do
		echo $FILE
		DIR="${FILE%\/*}";
		BASENAME="$(basename ${FILE})"
		FILENAME="${BASENAME%.*}"
		cd "$DIR";
		dune exec dlock -- -ds 1 -s -p "$(cat "$FILENAME.ccs" | tr '\n' ' ')" &> "$FILENAME".ccs.dlock.ds1 || true
		timeout 10 dune exec dlock -- -ds 2 -s -p "$(cat "$FILENAME.ccs" | tr '\n' ' ')" &> "$FILENAME".ccs.dlock.ds2 || true
		dune exec dlock-orig -- -ds 1 -s -p "$(cat "$FILENAME.ccs" | tr '\n' ' ')" &> "$FILENAME".ccs.dlock-orig.ds1 || true
		dune exec dlock-orig -- -ds 2 -s -p "$(cat "$FILENAME.ccs" | tr '\n' ' ')" &> "$FILENAME".ccs.dlock-orig.ds2 || true
		cd "$BASE_PATH"
	done

# list_files "main.go" $@ |
# 	while read FILE; do
# 		echo $FILE
# 		DIR="${FILE%\/*}";
# 
# 		docker run --rm -v "$BASE_PATH/$DIR":/playground/bug -v "$SCRIPT_DIR/gen_gcatch.sh:/playground/gen_gcatch.sh" gcatch /playground/gen_gcatch.sh | tee "$DIR"/main.gcatch
# 	done

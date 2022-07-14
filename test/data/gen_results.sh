#!/bin/bash -xe



BASE_PATH="$PWD"
find . -name "main.go" |
	while read file; do
		echo $file
		dir="${file%\/*}";
		cd "$dir";
		~/go/bin/migoinfer main.go > main.migo;
		cd "$BASE_PATH"
	done

find . -name "main.migo" |
	while read file; do
		echo $file
		dir="${file%\/*}";
		cd "$dir";
		docker run --rm -v "$PWD":/root/go nickng/gong:popl17ae Gong -A /root/go/main.migo &> main.gong || true
		docker run -t --rm -v $(pwd):/root jgabet/godel2:latest /bin/sh -c "Godel -T main.migo; Godel main.migo" &> main.godel2 || true
		dune exec dlock -- -ds 1 -s -m main.migo &> main.dlock.ds1 || true
		dune exec dlock -- -ds 1 -s -m main.migo &> main.dlock.ds2 || true
		dune exec dlock-orig -- -ds 1 -s -m main.migo &> main.dlock-orig.ds1 || true
		dune exec dlock-orig -- -ds 1 -s -m main.migo &> main.dlock-orig.ds2 || true
		cd "$BASE_PATH"
	done

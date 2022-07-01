#!/bin/bash -x

find . -name "main.go" |
	while read FILE; do
		~/go/bin/migoinfer "$FILE" > ${FILE%.go}.migo
	done

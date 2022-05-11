#!/bin/bash -e

CLONE_DIR=dlock-orig

if [ ! -d "$CLONE_DIR" ]; then
	git clone -b orig git@github.com:JorgeGCoelho/Deadlock-Detector-and-Resolver.git "$CLONE_DIR"
else
	git -C "$CLONE_DIR" pull
fi

./test.sh output_orig/ --root "$CLONE_DIR"
./test.sh
./compare.sh


#!/bin/bash -ex

mkdir -p src/main
cp bug/main.go src/main/main.go
export GOPATH=/playground/
time /go/bin/GCatch -compile-error -path=/playground/src/main/ -include="main" -checker=BMOC -r

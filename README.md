# Deadlock-Detector-and-Resolver
## A Tool for Static Deadlock Detection and Resolution in Go Programs

This repository contains a proof of concept tool developed for a Master's Thesis called "Automatic Resolution of Deadlocks in Concurrent Programs", by Afonso Almeida at NOVA School of Science and Technology.

### Run Instructions
- In order to compile this tool, compile it using\
`ocamlopt -g  -o dlock cmd.ml types.ml printer.ml auxfunctions.ml deadlock_detector.ml`
- The tool can take multiple arguments, defined by\ `./dlock [-v | -s | -ds] <file1>`

`-v` indicates a verbose output mode, '-s' indicates a simple output mode, and '-ds' can be set to 1 or 2, which indicates the resolution algorithm chosen.\
For instance, `./dlock -s -ds 1` sets the tool to output in a simpler form and to use the first resolution algorithm.

Multiple examples of processes are included in [`test_file`](https://github.com/afonsoalmeida/Deadlock-Detector-and-Resolver/blob/master/test_file.txt), which just need to be copied and pasted to [`deadlock_detector`](https://github.com/afonsoalmeida/Deadlock-Detector-and-Resolver/blob/master/deadlock_detector.ml) and run as argument of the `main` function.\
`deadlock_detector` already contains several examples ready to be run, commented.\
\
Several benchmarks used for evaluation in the thesis can be found in the folder [benchmarks](https://github.com/afonsoalmeida/Deadlock-Detector-and-Resolver/tree/master/benchmarks).

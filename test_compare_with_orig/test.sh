#!/bin/bash -e

if [ "$#" -eq 0 ]; then
	OUTPUT_DIR=output
else
	OUTPUT_DIR="$1"
	shift 1
fi


rm "$OUTPUT_DIR"/*.txt || true
mkdir -p "$OUTPUT_DIR"

while read line; do 
	name="${line%% *}"
	process="${line#* }"


	verbosity_v=("-v" "v")
	verbosity_d=("-s" "s")
	verbosity_x=("  " "-")
	verbosities=( verbosity_v verbosity_d verbosity_x)

	deadlock_algo_1=("-ds=1" "d1")
	deadlock_algo_2=("-ds=2" "d2")
	deadlock_algos=(deadlock_algo_1 deadlock_algo_2)

	declare -n verbosity deadlock_algo
	for verbosity in "${verbosities[@]}"; do
		for deadlock_algo in "${deadlock_algos[@]}"; do
			OUTPUT_FILE="$OUTPUT_DIR"/out_"$name-${verbosity[1]}-${deadlock_algo[1]}.txt"
			# echo "$OUTPUT_FILE ${verbosity[0]} ${deadlock_algo[0]} ${verbosity[1]} ${deadlock_algo[1]} $name $process"
			set -x 
			dune exec $@ -- dlock ${verbosity[0]} ${deadlock_algo[0]} -p "$process" > "$OUTPUT_FILE" 2>/dev/null
			{ set +x; } 2>/dev/null
		done
	done
done <<EOF
1   (a!.a?.0 || b?.b!.c?.c!.0) + c!.c?.0
1-1 (a!.a?.0 || b?.b!.0 || c?.c!.0) + c!.c?.0
1-2 (a!.a?.0 || b?.b!.0 || c?.c!.0) + (c!.c?.0 || d!.d?.0)
2   a!.0 || (b!.b?.a?.0 + a?.0)
3   (a!.0 || a?.0) + (b!.0 || b?.0) + (c!.0 || c?.0)
4   a?.(c?.0 + d?.0) || a!.e!.0
5   a!.(b!.c!.0 || b?.c?.d?.0) || a?.d!.0
EOF

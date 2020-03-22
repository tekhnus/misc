#!/bin/bash
if [ $# -ne 5 ]
then
    echo "Usage: $0 n time numstart numstep numend"
    exit
fi

run_one () {
    export OMP_NUM_THREADS=$3
    echo "{"
    echo "  \"numthreads\": $3,"
    echo "  \"timings\": {"
    echo -n "    \"static\": "
    ./heat_omp_static $1 $2
    echo -n "    \"dynamic\": "
    ./heat_omp_dynamic $1 $2
    echo -n "    \"guided\": "
    ./heat_omp_guided $1 $2
    echo "  }"
    echo "},"
}

echo "["
for numthreads in 1 $(seq $3 $4 $5)
do
    run_one $1 $2 $numthreads
done
echo "]"


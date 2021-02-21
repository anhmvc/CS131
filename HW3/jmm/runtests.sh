#!/bin/bash

classes=(Synchronized Unsynchronized AcmeSafe)
numThreads=(1 8 32 40)
size=(5 50 100)

for i in "${classes[@]}"; do
    for j in "${numThreads[@]}"; do
        for k in "${size[@]}"; do
            echo "time timeout 3600 java UnsafeMemory $i $j 100000000 $k";
            time timeout 3600 java UnsafeMemory $i $j 100000000 $k;
            echo "";
        done
    done
done

echo "Completed all tests"

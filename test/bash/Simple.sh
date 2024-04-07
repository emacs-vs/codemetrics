#!/bin/bash

function usage() {
    echo "Run with ./Simple.sh"
}

function print_numbers() {
    n=$1
    for i in $(seq 1 $n); do
        echo "$i"
    done
}

function print_numbers_rev() {
    n=$1
    while [[ $n -gt 0 ]]; do
        echo "$n"
        n=$(($n-1))
    done
}

if [[ $# -ne 0 ]]
then
    usage
    exit 1
fi

print_numbers 10

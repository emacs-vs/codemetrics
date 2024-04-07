#!/bin/bash

function factorial() {
    n=$1
    if [[ $n -le 1 ]]
    then
        echo "1"
    else
        next=$(factorial $(($n-1)))
        echo $(($n * $next))  
    fi
}

factorial $1

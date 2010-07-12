#!/bin/bash

markProgress() {
        if [ -z "$1" ] ; then
                echo -n "."
        else
                echo -n "x"
        fi
}



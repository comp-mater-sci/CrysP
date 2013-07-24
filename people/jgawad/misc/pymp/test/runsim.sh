#!/bin/bash

echo Running simulation
[[ -z $1 ]] && exit 1
tr -s [:cntrl:] > $1
pwd >> $1

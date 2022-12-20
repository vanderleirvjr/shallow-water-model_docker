#!/bin/bash

cd /home/container/src 

make

cd ../bin 

if [ ! -f "./namelist.input" ]; then
    cp -rf ../util/namelist.input .
fi
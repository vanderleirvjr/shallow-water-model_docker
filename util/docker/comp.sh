#!/bin/bash

cd /home/container/src 

source /home/container/.bashrc

make

cd ../bin 

if [ ! -f "./namelist.input" ]; then
    cp -rf ../util/namelist.input .
fi
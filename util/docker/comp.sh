#!/bin/bash

cd /home/container/src 

make

cd ../bin 

ln -fs ../src/swf.exe .

if [ ! -f "./namelist.input" ]; then
    cp -rf ../util/namelist.input .
fi
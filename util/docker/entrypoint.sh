#!/bin/bash

cd /home/container

echo "export LD_LIBRARY_PATH=/usr/local/lib:${LD_LIBRARY_PATH}" >> .bashrc 
echo "export PATH=/usr/local/bin:${PATH}" >> .bashrc 


tail -f /dev/null
#!/bin/bash 
clear 
echo "Updating all submodules for csound-extended..."
git submodule update --init --recursive
git submodule update --recursive
git submodule status --recursive
echo "Finished updating all submodules for csound-extended."
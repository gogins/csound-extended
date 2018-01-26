#!/bin/bash 
clear 
echo "Updating all submodules for Csound for Android..."
git submodule update --init --recursive
git submodule update --recursive
git submodule status --recursive
echo "Finished updating all submodules for Csound for Android."

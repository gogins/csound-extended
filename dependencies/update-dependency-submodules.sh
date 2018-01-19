#!/bin/bash 
clear 
echo "Updating all dependencies for csound-extended.."
git submodule update --init --recursive
git submodule update --recursive
git submodule status --recursive
echo "Finished updating all dependencies for csound-extended."

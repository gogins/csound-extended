#!/bin/bash 
clear 
echo "Updating all dependencies for csound-extended.."
git submodule init --recursive
git submodule status --recursive
echo "Finished updating all dependencies for csound-extended."

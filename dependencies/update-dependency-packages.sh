#!/bin/bash
echo "Updating all Debian packages required for csound-extended..."
echo
echo "Updating Eigen3..."
sudo apt-get install libeigen3-dev
echo "Updating FLTK..."
sudo apt-get install libfltk1.4-dev
sudo apt-get install libfltk-images1.4
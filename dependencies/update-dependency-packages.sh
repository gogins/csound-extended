#!/bin/bash
echo "Updating all Debian packages required for csound-extended..."
echo
echo "Updating Eigen3..."
sudo apt-get install libeigen3-dev
echo "Updating FLTK 1.3..."
sudo apt-get install libfltk1.3-dev
sudo apt-get install libfltk-images1.3
echo "Updating PortAudio..."
sudo apt-get install portaudio19-dev
echo "Updating PortMidi..."
sudo apt-get install libportmidi-dev
echo "Updating PortSMF..."
sudo apt-get install libportsmf-dev
echo "Finished updating all Debian packages required for csound-extended."

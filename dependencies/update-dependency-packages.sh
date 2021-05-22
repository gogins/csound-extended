#!/bin/bash
echo "Updating all Debian packages required for csound-extended..."
echo
apt-get update
apt-get upgrade
echo "Updating build-essential..."
apt-get install build-essential
echo "Updating automake..."
apt-get install automake
echo "Updating autoconf..."
apt-get install autoconf
echo "Updating autotools..."
apt-get install autotools
echo "Updating boost..."
apt-get install libboost-dev
apt-get install libboost-math1.71-dev
echo "Updating FFmpeg..."
apt-get install ffmpeg
apt-get install libavfilter-dev
apt-get install libmp3lame-dev
echo "Updating libtool..."
apt-get install libtool
echo "Updating autogen..."
apt-get install autogen
echo "Updating ecl..."
apt-get install ecl
echo "Updating libeigen3-dev..."
apt-get install libeigen3-dev
echo "Updating FLTK 1.3..."
apt-get install fluid
apt-get install libfltk1.3-dev
apt-get install libfltk-images1.3
echo "Updating fluidsynth..."
apt-get install libfluidsynth-dev
echo "Updating libgit2-dev..."
apt-get install libgit2-dev
echo "Updating gcc-arm-linux-gnueabi..."
apt-get install gcc-arm-linux-gnueabi
echo "Updating PortAudio..."
apt-get install portaudio19-dev
echo "Updating PortMidi..."
apt-get install libportmidi-dev
echo "Updating PortSMF..."
apt-get install libportsmf-dev
echo "Updating libsox-dev..."
apt-get install libsox-dev
echo "Updating gcc-aarch64-linux-gnu..."
apt-get install gcc-aarch64-linux-gnu
echo "Updating linux-libc-dev:i386..."
apt-get install linux-libc-dev:i386
echo "Updating LLVM..."
wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
apt-add-repository "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-5.0 main"
apt-get update
apt-get install -y clang-5.0
-k
echo "Updating python3.9-dev..."
apt-get install python3.9-dev
echo "Updating OpenCV..."
apt-get libopencv-dev --fix-missig
echo "Updating sbcl..."
apt-get install sbcl
echo "Finished updating all Debian packages required for csound-extended."

#!/bin/bash
echo "Updating all Debian packages required for csound-extended..."
echo
sudo apt update
sudo apt upgrade
echo "Updating build-essential..."
sudo apt install build-essential
echo "Updating automake..."
sudo apt install automake
echo "Updating autoconf..."
sudo apt install autoconf
echo "Updating autotools..."
sudo apt install autotools
echo "Updating libtool..."
sudo apt install libtool
echo "Updating autogen..."
sudo apt install autogen
echo "Updating csound..."
sudo apt install csound
sudo apt install csound-data
sudo apt install csound-doc
sudo apt install csound-utils
sudo apt install csoundqt
sudo apt install csoundqt-examples
sudo apt install libcsnd-dev
sudo apt install libcsound64-6.0
sudo apt install libcsound64-dev
sudo apt install multimedia-csound
sudo apt install python-csound
echo "Updating libeigen3-dev..."
sudo apt install libeigen3-dev
echo "Updating FLTK..."
sudo apt install fluid
sudo apt install libfltk1.1-dev
sudo apt install libfltk-images
echo "Updating fluidsynth..."
sudo apt install libfluidsynth-dev
echo "Updating gcc-arm-linux-gnueabi..."
sudo apt install gcc-arm-linux-gnueabi
echo "Updating git-buildpackage..."
sudo apt install git-buildpackage
echo "Updating PortAudio..."
sudo apt install portaudio19-dev
echo "Updating PortMidi..."
sudo apt install libportmidi-dev
echo "Updating PortSMF..."
sudo apt install libportsmf-dev
echo "Updating qt5base-dev..."
sudo apt install qt5base-dev
echo "Updating qtwebengine5-dev..."
sudo apt install qtwebengine5-dev
echo "Updating gcc-aarch64-linux-gnu..."
sudo apt install gcc-aarch64-linux-gnu
echo "Updating linux-libc-dev:i386..."
sudo apt install linux-libc-dev:i386
echo "Updating LLVM..."
wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | sudo apt-key add -
sudo apt-add-repository "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-5.0 main"
sudo apt update
sudo apt install -y clang-5.0
echo "Updating cl-rsm-mod..."
sudo apt install cl-rsm-mod
echo "Updating libatomic-ops-dev..."
sudo apt install libatomic-ops-dev
echo "Updating Embeddable Common Lisp..."
sudo apt install ecl
echo "Updating STK..."
sudo apt install libstk0-dev
echo "Finished updating all Debian packages required for csound-extended."

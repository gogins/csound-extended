#!/bin/bash
echo "Pulling from submodule repositories..."
cd csound
git checkout develop
echo "Pulling from `pwd`..."
git pull
cd ..
cd eigen
echo "Pulling from `pwd`..."
git pull
cd ..
cd liblo
echo "Pulling from `pwd`..."
git pull
cd ..
cd libmusicxml
echo "Pulling from `pwd`..."
git pull
cd ..
cd libsndfile
echo "Pulling from `pwd`..."
git pull
cd ..
cd link
echo "Pulling from `pwd`..."
git pull
cd ..
cd luajit-2.0
echo "Pulling from `pwd`..."
git pull
cd ..
cd oboe
echo "Pulling from `pwd`..."
git pull
cd ..
cd portaudio
echo "Pulling from `pwd`..."
git pull
cd ..
cd stk
echo "Pulling from `pwd`..."
git pull
cd ..
echo "Finished pulling from submodule repositories."



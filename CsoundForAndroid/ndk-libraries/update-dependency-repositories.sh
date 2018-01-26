#!/bin/bash
echo "Pulling from submodule repositories for CsoundForAndroid..."
cd fluidsynth-android
echo "Pulling from `pwd`..."
git checkout master
git pull
cd ..
cd liblo-android
echo "Pulling from `pwd`..."
git checkout master
git pull
cd ..
cd libsndfile-android
echo "Pulling from `pwd`..."
git checkout master
git pull
cd ..
cd link
echo "Pulling from `pwd`..."
git checkout master
git pull
cd ..
cd luajit-2.0
echo "Pulling from `pwd`..."
git checkout v2.1
git pull
cd ..
cd oboe
echo "Pulling from `pwd`..."
git checkout master
git pull
cd ..
cd stk
echo "Pulling from `pwd`..."
git checkout master
git pull
cd ..
echo "Finished pulling from submodule repositories for CsoundForAndroid."



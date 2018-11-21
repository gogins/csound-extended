#!/bin/bash
echo "Pulling from submodule repositories..."
cd fluidsynth-android
echo "pulling from `pwd`"    
git checkout master
git pull
cd ..
cd liblo-android
echo "pulling from `pwd`"    
git checkout master
git pull
cd ..
cd libsndfile-android
echo "pulling from `pwd`"    
git checkout master
git pull
cd ..
cd link
echo "pulling from `pwd`"    
git checkout master
git pull
cd ..
cd luajit-2.0
echo "pulling from `pwd`"    
git checkout v2.1
git pull
cd ..
cd oboe
echo "pulling from `pwd`"    
git checkout master
git pull
cd ..
cd stk
echo "pulling from `pwd`"    
git checkout master
git pull
cd ..
cd ../../dependencies/csound
echo "pulling from `pwd`"    
git checkout develop
git pull
cd ../../CsoundForAndroid
echo "Finished pulling from submodule repositories."
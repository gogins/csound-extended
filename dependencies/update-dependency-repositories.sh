#!/bin/bash
echo "Pulling from submodule repositories..."
cd bformdec2
echo "Pulling from `pwd`..."
git checkout master
git pull 
git branch
cd ..
cd cmask
echo "Pulling from `pwd`..."
git checkout master
git pull 
git branch
cd ..
cd cope/david-cope-cmmc
echo "Pulling from `pwd`..."
git checkout master
git pull 
git branch
cd ../..
cd csound
echo "Pulling from `pwd`..."
git checkout develop
git pull 
git branch
cd ..
##cd faust
##echo "Pulling from `pwd`..."
##git checkout master-dev
##git pull
##  cd ..
cd fomus
echo "Pulling from `pwd`..."
git checkout master
git pull
cd ..
cd cm-fomus
echo "Pulling from `pwd`..."
git checkout master
git pull
cd ..
cd liblo
echo "Pulling from `pwd`..."
git checkout master
git pull
git branch
cd ..
cd libmusicxml
echo "Pulling from `pwd`..."
git checkout master
git pull
git branch
cd ..
cd libsndfile
echo "Pulling from `pwd`..."
git checkout master
git pull
git branch
cd ..
cd link
echo "Pulling from `pwd`..."
git checkout master
git pull
git branch
cd ..
cd oboe
echo "Pulling from `pwd`..."
#git checkout addinput
git checkout master
git pull
git branch
cd ..
cd portaudio
echo "Pulling from `pwd`..."
git checkout master
git pull
git branch
cd ..
cd stk
echo "Pulling from `pwd`..."
git checkout master
git pull
git branch
cd ..
cd weyl
echo "Pulling from `pwd`..."
git checkout master
git pull
git branch
cd ..
sudo -k
echo "Finished pulling from submodule repositories."
echo "Back in `pwd`."
git branch


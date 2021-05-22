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
cd libmusicxml
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
sudo -k
echo "Finished pulling from submodule repositories."
echo "Back in `pwd`."
git branch


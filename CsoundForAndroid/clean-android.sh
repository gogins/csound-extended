#!/bin/bash
echo "Cleaning all Linux artifacts from `pwd`..."
find . -wholename "ndk-libraries/*.o" -ls
find . -wholename "ndk-libraries/*.a" -ls
find . -wholename "ndk-libraries/*.so" -ls
cd ..

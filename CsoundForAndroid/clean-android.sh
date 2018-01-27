#!/bin/bash
echo "Cleaning all Linux artifacts from `pwd`..."
find ndk-libraries -name "*.o" -delete
find ndk-libraries -name "*.a" -delete
find ndk-libraries -name "*.so" -delete
cd ..

#!/bin/bash
echo "Building all native libraries for Android..."
echo
if [ -z "$ANDROID_NDK_ROOT" ]; then
    echo "ERROR: ANDROID_NDK_ROOT is not set. Please set this variable to point to the root directory of your Android Native Development Kit installation to continue.";
    exit;
fi
if [ -z "ANDROID_SDK_ROOT" ]; then
    echo "ERROR: ANDROID_SDK_ROOT is not set. Please set this variable to point to the root directory of your Android SDK installation to continue.";
    exit;
fi
if [ -z "CSOUND_SRC_ROOT" ]; then
    echo "ERROR: CSOUND_SRC_ROOT is not set. Please set this variable to point to the dependencies/csound directory of this project to continue.";
    exit;
fi
if [ -z "NDK_MODULE_PATH" ]; then
    echo "ERROR: NDK_MODULE_PATH is not set. Please set this variable to point to the ndk-libraries directory of this project to continue.";
    exit;
fi
MACHINE="$(uname -s)"
case "${MACHINE}" in 
  MINGW*) NDK_BUILD_CMD=$ANDROID_NDK_ROOT/ndk-build.cmd;;
  *) NDK_BUILD_CMD="$ANDROID_NDK_ROOT/ndk-build -j6"
esac
echo "NDK_BUILD_COMMAND: $NDK_BUILD_CMD"
echo

cd ndk-libraries

cd doppler-opcodes
echo "Building `pwd`..."
$NDK_BUILD_CMD $1
if [ $? -eq 0 ]; then
    echo OK
else
    echo "Not building `pwd` library..."
fi
cd ..

cd fluidsynth-opcodes
echo "Building `pwd`..."
$NDK_BUILD_CMD $1
if [ $? -eq 0 ]; then
    echo OK
else
    echo "Not building `pwd` library..."
fi
cd ..

cd link-opcodes
echo "Building `pwd`..."
$NDK_BUILD_CMD $1
if [ $? -eq 0 ]; then
    echo OK
else
    echo "Not building `pwd` library..."
fi
cd ..

cd oboe-csound
echo "Building `pwd`..."
$NDK_BUILD_CMD $1
if [ $? -eq 0 ]; then
    echo OK
else
    echo "Not building `pwd` library..."
fi
cd ..

cd osc-opcodes
echo "Building `pwd`..."
$NDK_BUILD_CMD $1
if [ $? -eq 0 ]; then
    echo OK
else
    echo "Not building `pwd` library..."
fi
cd ..

cd scansyn-opcodes
echo "Building `pwd`..."
$NDK_BUILD_CMD $1
if [ $? -eq 0 ]; then
    echo OK
else
    echo "Not building `pwd` library..."
fi
cd ..

cd signalflowgraph-opcodes
echo "Building `pwd`..."
$NDK_BUILD_CMD $1
if [ $? -eq 0 ]; then
    echo OK
else
    echo "Not building `pwd` library..."
fi
cd ..

cd stdutil-opcodes
echo "Building `pwd`..."
$NDK_BUILD_CMD $1
if [ $? -eq 0 ]; then
    echo OK
else
    echo "Not building `pwd` library..."
fi
cd ..

cd plugin-opcodes
echo "Building `pwd`..."
$NDK_BUILD_CMD $1
if [ $? -eq 0 ]; then
    echo OK
else
    echo "Not building `pwd` library..."
fi
cd ..

cd stk-opcodes
echo "Building `pwd`..."
$NDK_BUILD_CMD $1
if [ $? -eq 0 ]; then
    echo OK
else
    echo "Not building `pwd` library..."
fi
cd ..

cd ..

echo "Building CsoundAndroid library..."

cd CsoundAndroid

bash build.sh $1
if [ $? -eq 0 ]; then
    echo OK
else
    echo FAIL
    exit
fi 
cd ..

echo "Installing binaries for the Csound for Android app..."
rm -rf CsoundForAndroid/CsoundAndroid/src/main/java/csnd6
cp -r CsoundAndroid/src/csnd  CsoundForAndroid/CsoundAndroid/src/main/java/
rm -rf CsoundAndroid/src/csnd
rm -rf CsoundForAndroid/CsoundAndroid/src/main/jniLibs
cp -r CsoundAndroid/libs  CsoundForAndroid/CsoundAndroid/src/main/jniLibs
cd CsoundForAndroid/CsoundApplication
bash install_libs.sh
cd ..
cd ..


echo "Finished building all native libraries for Android."

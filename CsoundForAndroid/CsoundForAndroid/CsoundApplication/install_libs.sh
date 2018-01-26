#!/bin/bash

# Simple script to copy "local" shared libs to the installation libs directories.
# This because I can't seem to get the canonical way to do this to work.

JNILIBS=src/main/jniLibs

mkdir -p $JNILIBS 
mkdir -p $JNILIBS/arm64-v8a
mkdir -p $JNILIBS/armeabi-v7a

find $NDK_MODULE_PATH -wholename "*/libs/arm64-v8a/*.so" -exec cp -f {} $JNILIBS/arm64-v8a/ ';'
find $NDK_MODULE_PATH -wholename "*/libs/armeabi-v7a/*.so" -exec cp -f {} $JNILIBS/armeabi-v7a/ ';'

rm -f $JNILIBS/arm64-v8a/libsndfile.so
rm -f $JNILIBS/armeabi-v7a/libsndfile.so
rm -f $JNILIBS/arm64-v8a/libgnustl_shared.so
rm -f $JNILIBS/armeabi-v7a/libgnustl_shared.so

echo "These are the built and copied libs for the Csound for Android app:"
find ../../CsoundAndroid/libs -name "*.so" -ls
find $JNILIBS -name "*.so" -ls

# Also copy other resources used by Csound opcodes.

mkdir -p src/main/assets/samples/
cp -f ../../../samples/* src/main/assets/samples




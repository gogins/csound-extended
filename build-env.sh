#!/bin/bash
echo "Configure for Android/ARM..."

# The idea is we want to shrink this mess, so we unset each variable,
# try commenting it out to see if we need it, and export it if we actually
# do need it.

unset CSOUND_HOME
#export CSOUND_HOME=/home/mkg/csound/csound
unset CSOUND_SRC_ROOT
export CSOUND_SRC_ROOT=/home/mkg/csound-extended/dependencies/csound
#export CSOUND_HOME=/home/mkg/csound/csound
unset NDK_MODULE_PATH
export NDK_MODULE_PATH=/home/mkg/csound-extended/CsoundForAndroid/ndk-libraries
unset ANDROID_NDK_ROOT
export ANDROID_NDK_ROOT=/home/mkg/Android/Sdk/ndk-bundle
unset ANDROID_STUDIO_ROOT
#export ANDROID_STUDIO_ROOT=/home/mkg/Android/android-studio
unset ANDROID_ROOT
export ANDROID_ROOT=/home/mkg/Android
unset NODE_PATH
export NODE_PATH=/home/mkg/csound-extended/csound.node/build/Release
unset ANDROID_SDK_ROOT
export ANDROID_SDK_ROOT=/home/mkg/Android/Sdk
export ANDROID_HOME=$ANDROID_SDK_ROOT

unset SADIR
export SADIR=$CSOUND_SRC_ROOT/samples/
unset SSDIR
export SSDIR=/home/mkg/michael.gogins.studio/music/samples/
unset INCDIR
export INCDIR=/home/mkg/csound-extended/silencio/patches/
## source ./emsdk/emsdk_env.sh --build=Release
export PATH=${PATH}:/opt/android-studio/bin:/home/mkg/Android/Sdk/platform-tools
# For node-gyp:
export PATH=/usr/local/lib/node-v12.14.1-linux-x64/bin:${PATH}
unset NODE_ADDON_API_INCLUDE
export NODE_ADDON_API_INCLUDE=/usr/local/lib/node-v12.14.1-linux-x64/lib/node_modules/node-addon-api

unset OPCODE6DIR64
export OPCODE6DIR64=/usr/lib/csound/plugins64-6.0/

unset ALSA_CARD
export ALSA_CARD=USB

env | grep CSOUND
env | grep ANDROID
env | grep NDK
env | grep NODE
env | grep QT
env | grep DIR

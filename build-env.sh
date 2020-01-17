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

unset LUA_PATH
export LUA_PATH=/home/mkg/csound-extended/silencio/lua/?.lua
unset LUA_CPATH
export LUA_CPATH=/usr/lib/lua/5.1/?.so


unset QT_SELECT
export QT_SELECT=qt5
unset SADIR
export SADIR=$CSOUND_SRC_ROOT/samples
unset SSDIR
export SSDIR=/home/mkg/michael.gogins.studio/music/samples
unset INCDIR
export INCDIR=/home/mkg/csound/gogins.github.io/csound/silencio/patches
## source ./emsdk/emsdk_env.sh --build=Release
export PATH=${PATH}:/opt/android-studio/bin:/home/mkg/Android/Sdk/platform-tools:/usr/lib/node_modules/npm/bin/node-gyp-bin
# This better be, and remain, the one true nodejs.
export PATH=/usr/local/lib/node-v12.14.1-linux-x64/bin:${PATH}
##export PATH=/usr/lib/node_modules/npm/node_modules/node-gyp/bin:${PATH}
unset CSOUND_EXTENDED_VERSION
export CSOUND_EXTENDED_VERSION=1.0.2

unset OPCODE6DIR64
set OPCODE6DIR64=/usr/lib/csound/plugins64-6.0

unset ALSA_CARD
export ALSA_CARD=USB

env | grep CSOUND
env | grep ANDROID
env | grep NDK
env | grep NODE
env | grep QT

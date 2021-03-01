#!/bin/bash
echo "Configure for Android/ARM..."

# The idea is we want to shrink this mess, so we unset each variable,
# try commenting it out to see if we need it, and export it if we actually
# do need it.

unset CSOUND_HOME
#export CSOUND_HOME=/home/mkg/csound/csound
unset CSOUND_SRC_ROOT
export CSOUND_SRC_ROOT=/home/mkg/csound-android/dependencies/csound
#export CSOUND_HOME=/home/mkg/csound/csound

unset NDK_MODULE_PATH
export NDK_MODULE_PATH=/home/mkg/csound-android/CsoundForAndroid/ndk-libraries
unset ANDROID_NDK_ROOT
export ANDROID_NDK_ROOT=/home/mkg/Android/Sdk/ndk/22.0.7026061
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
export LUA_CPATH=/usr/local/lib/?.so

unset SADIR
export SADIR=$CSOUND_SRC_ROOT/samples
unset SSDIR
export SSDIR=/home/mkg/michael.gogins.studio/music/samples/
unset INCDIR
export INCDIR=/home/mkg/csound-extended/silencio/patches/
## source ./emsdk/emsdk_env.sh --build=Release
export PATH=${PATH}:/opt/android-studio/bin:/home/mkg/Android/Sdk/platform-tools:/usr/lib/node_modules/npm/bin/node-gyp-bin
## export PATH=/usr/lib/node_modules/npm/node_modules/node-gyp/bin:${PATH}
unset CSOUND_EXTENDED_VERSION
export CSOUND_EXTENDED_VERSION=1.1.0

unset OPCODE6DIR64
export OPCODE6DIR64=/usr/local/lib/csound/plugins64-6.0:/usr/lib/csound/plugins64-6.0:/home/mkg/csound-vst3-opcodes/build/lib/Debug

unset ALSA_CARD
export ALSA_CARD=USB

# Node.js

unset NODEJS_VERSION
export NODEJS_VERSION=v13.6.0
unset NODEJS_DISTRO
export NODEJS_DISTRO=linux-x64
export PATH=/usr/local/lib/nodejs/node-$NODEJS_VERSION-$NODEJS_DISTRO/bin:$PATH
unset NODE_ADDON_API_INCLUDE
export NODE_ADDON_API_INCLUDE=/usr/local/lib/nodejs/node-$NODEJS_VERSION-$NODEJS_DISTRO/lib/node_modules/node-addon-api

alias python=python3

unset EMSCRIPTEN_ROOT
export EMSCRIPTEN_ROOT=/home/mkg/emsdk/upstream/emscripten

unset RAWWAVE_PATH
export RAWWAVE_PATH=/home/mkg/stk/rawwaves/  

# Install Ruby Gems to ~/gems'

unset GEM_HOME
export GEM_HOME="$HOME/gems"
export PATH="$HOME/gems/bin:$PATH"

unset CHROMIUM_USER_FLAGS
# export CHROMIUM_USER_FLAGS=""

unset APULSE_PLAYBACK_DEVICE
export APULSE_PLAYBACK_DEVICE=plughw:1,0
unset APULSE_CAPTURE_DEVICE
export APULSE_CAPTURE_DEVICE=plughw:1,0

alias python='/usr/bin/python3.9'

echo $PATH
python --version
env | grep APULSE
env | grep CHROMIUM
env | grep CSOUND
env | grep ANDROID
env | grep NDK
env | grep NODE
env | grep OPCODE
env | grep PYTHON
env | grep RAW

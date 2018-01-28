#!/bin/sh

export ANDROID_NDK_ROOT=$NDK

$ANDROID_NDK_ROOT/ndk-build TARGET_PLATFORM=android-9 $@




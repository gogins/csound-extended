#/bin/bash

echo "Building Embeddable Common Lisp host for Android..."

# Please note, the original instructions were for 32 bit CPU architecture. 
# Here, we want the following ABIs:
# - armeabi-v7a (32 bit architecture)
# - arm64-v8a (64 bit architecture)
# - x86_64 (64 bit architecture)

export NDK_PATH=$ANDROID_NDK_ROOT
export ANDROID_API=22 # Was 23
export TOOLCHAIN_PATH=$ANDROID_NDK_ROOT/toolchains/llvm/prebuilt/linux-x86_64
export SYSROOT=${TOOLCHAIN_PATH}/sysroot
# Had to add "host" ECL to PATH.
export PATH=$TOOLCHAIN_PATH/bin:`pwd`/ecl-android-host/bin:$PATH
ls -ll $TOOLCHAIN_PATH/bin

cd ../ecl
./configure ABI=32 CFLAGS="-m32 -g -O2" LDFLAGS="-m32 -g -O2" --prefix=`pwd`/ecl-android-host --disable-longdouble
make -j9
make install
rm -r build

cd ../ecl-csound

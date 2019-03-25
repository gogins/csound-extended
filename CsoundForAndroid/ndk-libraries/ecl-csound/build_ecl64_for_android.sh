#/bin/bash

cd ../ecl

export ECL_TO_RUN=`pwd`/ecl32-android-host/bin/ecl


export NDK_PATH=$ANDROID_NDK_ROOT
export ANDROID_API=22 # Was 23
export TOOLCHAIN_PATH=$ANDROID_NDK_ROOT/toolchains/llvm/prebuilt/linux-x86_64
export SYSROOT=${TOOLCHAIN_PATH}/sysroot
export PATH=$TOOLCHAIN_PATH/bin:$PATH

echo "Building Embeddable Common Lisp library for Android..."

export LDFLAGS="--sysroot=${SYSROOT} -D__ANDROID_API__=${ANDROID_API} -fuse-ld=bfd"
export CPPFLAGS="--sysroot=${SYSROOT} -D__ANDROID_API__=${ANDROID_API} -isystem ${SYSROOT}/usr/include/arm-linux-androideabi"
# export CC=arm-linux-androideabi-clang 
export CC=armv7a-linux-androideabi22-clang 
./configure --host=arm-linux-androideabi --prefix=`pwd`/ecl32-android --with-cross-config=`pwd`/src/util/android-arm.cross_config
make -j9
make install

echo "Embeddable Common Lisp library and assets in the ecl-android directory are ready to run on the Android system."

cd ../ecl-csound

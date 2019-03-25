#/bin/bash

echo "Building Embeddable Common Lisp host for Android..."

# Please note, the original instructions were for 32 bit CPU architecture. 
# Here, we want the following ABIs:
# - armeabi-v7a (32 bit architecture)
# - arm64-v8a (64 bit architecture)
# - x86_64 (64 bit architecture)

cd ../ecl
#./configure ABI=32 CFLAGS="-m32 -g -O2" LDFLAGS="-m32 -g -O2" --prefix=`pwd`/ecl64-android-host --disable-longdouble
./configure ABI=32 CFLAGS="-g -O2" LDFLAGS="-g -O2" --prefix=`pwd`/ecl64-android-host --disable-longdouble
make -j9
make install
rm -r build
export ECL_TO_RUN=`pwd`/ecl64-android-host/bin/ecl

# This information is obsolete.

#echo "Configuring Android toolchain for Embeddable Common Lisp..."
#export NDK_PATH=$ANDROID_NDK_ROOT
#export ANDROID_API=22 # Was 23
#export TOOLCHAIN_PATH=`pwd`/android-toolchain
#${NDK_PATH}/build/tools/make_standalone_toolchain.py --arch arm --install-dir ${TOOLCHAIN_PATH} --api ${ANDROID_API}
#export SYSROOT=${TOOLCHAIN_PATH}/sysroot
#export PATH=${TOOLCHAIN_PATH}/bin:$PATH

echo "Building Embeddable Common Lisp library for Android..."

export LDFLAGS="--sysroot=${SYSROOT} -D__ANDROID_API__=${ANDROID_API} -fuse-ld=bfd"
export CPPFLAGS="--sysroot=${SYSROOT} -D__ANDROID_API__=${ANDROID_API} -isystem ${SYSROOT}/usr/include/arm-linux-androideabi"
export CC=arm-linux-androideabi-clang 
./configure --host=arm-linux-androideabi --prefix=`pwd`/ecl-android --with-cross-config=`pwd`/src/util/android-arm64.cross_config
make -j9
make install

echo "Embeddable Common Lisp library and assets in the ecl-android directory are ready to run on the Android system."

cd ../ecl-csound

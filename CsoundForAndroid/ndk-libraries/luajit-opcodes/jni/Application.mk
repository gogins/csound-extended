APP_ABI := armeabi-v7a arm64-v8a x86_64
APP_CPPFLAGS += -fexceptions -frtti -std=c++11
APP_OPTIM := release
APP_PLATFORM := android-21
# LuaJIT does not currently build 
# for Android arm with clang.
# Both LuaJIT and LuaCsound must be built with gcc,
# and LuaJIT must be linked statically with LuaCsound.
# However, gnustl is no longer support for Android.
#APP_STL := gnustl_static
#NDK_TOOLCHAIN_VERSION := 4.9
APP_STL := c++_shared
NDK_TOOLCHAIN_VERSION := clang

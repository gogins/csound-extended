cmake_minimum_required(VERSION 2.8)

project(csound-extended)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_FLAGS "-std=c++11")
set(CPPSOUND_SOURCE_DIRECTORY "../dependencies/csound/interfaces")
set(CSOUND_INCLUDE_DIRECTORY "../dependencies/csound/include")
set(EIGEN_INCLUDE_DIRECTORY "../dependencies")
set(SNDFILE_INCLUDE_DIRECTORY "${CMAKE_SOURCE_DIR}/deps/libsndfile-1.0.25/src")
set(CMAKE_VERBOSE_MAKEFILE ON CACHE BOOL "Verbose makefile" FORCE)
set(BUILD_PLUGINS_"plugins" CACHE DIR "plugins directory" FORCE)
set(USE_COMPILER_OPTIMIZATIONS 0)
set(WASM 1)
set(INIT_STATIC_MODULES ON CACHE BOOL "Initialize static modules" FORCE)
set(USE_DOUBLE OFF CACHE BOOL "Use doubles for audio sample words" FORCE)
set(BUILD_MULTI_CORE OFF CACHE BOOL "Build for multi-core performance" FORCE)
set(BUILD_JACK_OPCODES OFF CACHE BOOL "Build Jacko opcodes" FORCE)
set(EMSCRIPTEN ON CACHE BOOL "Using Emscripten to build" FORCE)
set(CMAKE_TOOLCHAIN_FILE "$EMSCRIPTEN_ROOT/cmake/Modules/Platform/Emscripten.cmake")
set(CMAKE_MODULE_PATH "$EMSCRIPTEN_ROOT/cmake")
set(CMAKE_BUILD_TYPE Release)
set(HAVE_BIG_ENDIAN OFF CACHE BOOL "Have big-endian architecture" FORCE)
set(CMAKE_16BIT_TYPE "unsigned short")
set(HAVE_STRTOD_L OFF CACHE BOOL "Have strtod_l "FORCE)
set(BUILD_STATIC_LIBRARY ON CACHE BOOL "Build as static library" FORCE)
set(HAVE_ATOMIC_BUILTIN OFF CACHE BOOL "Have atomic buildins" FORCE)
set(HAVE_SPRINTF_L OFF CACHE BOOL "Have sprintf_l" FORCE)
set(USE_GETTEXT OFF CACHE BOOL "Use gettext" FORCE)
set(BUILD_UTILITIES OFF CACHE "Build Csound utility programs" BOOL )
set(SNDFILE_H_PATH "${CMAKE_SOURCE_DIR}/deps/libsndfile-1.0.25/src" CACHE PATH "sndfile.h directory" FORCE)
set(LIBSNDFILE_LIBRARY 
    "${CMAKE_SOURCE_DIR}/deps/lib/libvorbisfile.a"
    "${CMAKE_SOURCE_DIR}/deps/lib/libvorbis.a"
    "${CMAKE_SOURCE_DIR}/deps/lib/libsndfile.a"
    "${CMAKE_SOURCE_DIR}/deps/lib/libogg.a"
    "${CMAKE_SOURCE_DIR}/deps/lib/libFLAC.a"
    "${CMAKE_SOURCE_DIR}/deps/lib/libFLAC++.a"
    "${CMAKE_SOURCE_DIR}/deps/lib/libvorbisenc.a"
    )


add_subdirectory(cmask)

add_subdirectory(../dependencies/csound csound)

add_subdirectory(CsoundAC)


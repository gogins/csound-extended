cmake_minimum_required(VERSION 3.5)

message("CMAKE BUILD SYSTEM FOR CSOUND-EXTENDED")
message("Execute \"cd dependencies;./update-dependencies.sh;cd..\" before build!")
message("Repeat only when dependencies change.")
message("There are NO configuration options in this build.")
message("But if there ARE configuration variables, set them here.")

project(csound-extended)

set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/csound/cmake/Modules/" ${CMAKE_MODULE_PATH})
message(STATUS "CMAKE_MODULE_PATH: ${CMAKE_MODULE_PATH}")

set(BUILD_CSOUND_VST "ON")
set(CMAKE_BUILD_TYPE "RelWithDebInfo")
set(VSTSDK2X_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/dependencies/VST2_SDK")
option(USE_LIB64 "Set to on to set installation directory for libraries to lib64" OFF)
if(USE_LIB64)
    set(LIBRARY_INSTALL_DIR "lib64")
    add_definitions("-DLIB64")
else()
    set(LIBRARY_INSTALL_DIR "lib")
endif()
message(STATUS "LIBRARY INSTALL DIR: ${LIBRARY_INSTALL_DIR}")

find_package(FLTK)
find_package(Java)
find_package(JNI)
# We are building this ourselves.
# find_package(LIBLO)
 find_package(LUAJIT)
find_package(MUSICXML)
find_package(PORTSMF)
find_package(PythonLibs 2.7)
#include(ExternalProject)
#ExternalProject_Add(libantlr
#    SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/libantlr
#    CONFIGURE_COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/libantlr/configure --prefix=<INSTALL_DIR>
#    BUILD_COMMAND ${MAKE})
find_package(STK)
find_package(SWIG)
find_package(VSTSDK2X)

# Includes ExternalProject dependencies based on make not CMake.
add_subdirectory(dependencies)
add_subdirectory(dependencies/csound)
add_subdirectory(dependencies/liblo)
add_subdirectory(dependencies/libmusicxml)
add_subdirectory(dependencies/libsndfile)
add_subdirectory(dependencies/link)
add_subdirectory(dependencies/portaudio)
# We hope this is not needed.
# add_subdirectory(dependencies/stk)
add_subdirectory(CsoundAC)
# https://wiki.qt.io/Using_CMake_build_system
# http://doc.qt.io/qt-5/cmake-manual.html
add_subdirectory(CsoundHtml5)
add_subdirectory(CsoundVst)

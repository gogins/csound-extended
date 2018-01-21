cmake_minimum_required(VERSION 3.5)

message("CMAKE BUILD SYSTEM FOR CSOUND-EXTENDED")
message("Execute \"cd dependencies;./update-dependencies.sh;cd..\" before build!")
message("Repeat only when dependencies change.")
message("There are NO configuration options in this build.")
message("But if there ARE configuration variables, set them here.")

project(csound-extended)
include(ExternalProject)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules")
message(STATUS "CMAKE_MODULE_PATH: ${CMAKE_MODULE_PATH}")

set(CMAKE_BUILD_TYPE "RelWithDebInfo")
set(ALLOW_DUPLICATE_CUSTOM_TARGETS "Yes")

set(BUILD_CSOUND_VST "ON")
set(BUILD_LINEAR_ALGEBRA_OPCODES "ON")

set(VSTSDK2X_INCLUDE_DIR "${CMAKE_SOURCE_DIR}/dependencies/VST_SDK/VST2_SDK")
message(STATUS "VSTSDK2X_INCLUDE_DIR: ${VSTSDK2X_INCLUDE_DIR}")
option(USE_LIB64 "Set to on to set installation directory for libraries to lib64" ON)
if(USE_LIB64)
    set(LIBRARY_INSTALL_DIR "lib64")
    add_definitions("-DLIB64")
else()
    set(LIBRARY_INSTALL_DIR "lib")
endif()
message(STATUS "LIBRARY INSTALL DIR: ${LIBRARY_INSTALL_DIR}")

find_package(FLTK CONFIG REQUIRED)
find_package(Java)
find_package(JNI)
# We are building these here.
# find_package(LIBLO)
# find_package(LUAJIT)
# find_package(MUSICXML)
find_package(PORTSMF)
find_package(PythonLibs 2.7)
find_package(Qt5 COMPONENTS Core Widgets REQUIRED HINTS "~/Qt-5.8.0")
find_package(STK)
find_package(SWIG)

# Includes ExternalProject dependencies based on make not CMake.
add_subdirectory(dependencies)
add_subdirectory(dependencies/csound)
add_subdirectory(dependencies/liblo/cmake)
add_subdirectory(dependencies/libmusicxml/build)
#add_subdirectory(dependencies/libsndfile)
add_subdirectory(dependencies/link)
add_subdirectory(dependencies/portaudio)
# We hope this is not needed.
# add_subdirectory(dependencies/stk)
add_subdirectory(CsoundAC)
ExternalProject_Add(CsoundHtml5
    SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/CsoundHtml5"
    BUILD_IN_SOURCE "1"
    CONFIGURE_COMMAND "${QMAKE}"
    BUILD_COMMAND "${MAKE}")
add_subdirectory(CsoundVst)

#add_dependencies("CsoundAC" "csound")
#add_dependencies("CsoundHtml5" "csound")
#add_dependencies("csound.node" "csound")
#add_dependencies("csoundvstmain" "csound")

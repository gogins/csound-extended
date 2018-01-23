cmake_minimum_required(VERSION 3.5)

message("CMAKE BUILD SYSTEM FOR CSOUND-EXTENDED")
message("Execute \"cd dependencies;./update-dependencies.sh;cd..\" before build!")
message("Repeat only when dependencies change.")
message("There are NO configuration options in this build.")
message("But if there ARE configuration variables, set them here.")

# A certain amount of the following has been copied from the core Csound project,
# and hoisted up here to enable paths etc. to be set for all projects here.

project(csound-extended)

# These are the real targets: the packages.

set (CPACK_GENERATOR TGZ; DEB)
set (CPACK_DEBIAN_PACKAGE_ARCHITECTURE "`dpkg --print-architecture`")
set (CPACK_DEBIAN_PACKAGE_MAINTAINER "csound-devel@listserv.heanet.ie")
set (CPACK_DEBIAN_PACKAGE_DESCRIPTION "Csound is a sound and music processing language.")

# Top-level CMake configuration.

include(ExternalProject)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_PREFIX_PATH "${CMAKE_SOURCE_DIR}/local-linux")
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules")
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/dependencies/csound/cmake/Modules")
message(STATUS "CMAKE_MODULE_PATH: ${CMAKE_MODULE_PATH}")
set(CMAKE_BUILD_TYPE "RelWithDebInfo")
set(ALLOW_DUPLICATE_CUSTOM_TARGETS "Yes")
add_definitions(-DUSE_DOUBLE)
set(USE_DOUBLE "Yes")
configure_file(${CMAKE_SOURCE_DIR}/dependencies/csound/include/float-version.h.in ${CMAKE_CURRENT_BINARY_DIR}/float-version.h)
set(QT_INSTALL_PREFIX "~/Qt/5.10.0/gcc_64")
set(QMAKE "/home/mkg/Qt/5.10.0/gcc_64/bin/qmake")
set(CSOUND_INCLUDE_PATH "${CMAKE_SOURCE_DIR}/dependencies/csound/include")
message(STATUS "CSOUND_INCLUDE_PATH: ${CSOUND_INCLUDE_PATH}")
set(VSTSDK2X_INCLUDE_DIR "${CMAKE_SOURCE_DIR}/dependencies/VST_SDK/VST2_SDK")
message(STATUS "VSTSDK2X_INCLUDE_DIR: ${VSTSDK2X_INCLUDE_DIR}")
set(BUILD_DIR ${CMAKE_CURRENT_BINARY_DIR})
set(BUILD_PLUGINS_DIR ${BUILD_DIR})
set(BUILD_BIN_DIR ${BUILD_DIR})
set(BUILD_LIB_DIR ${BUILD_DIR})
#if(APPLE AND NOT ${CMAKE_GENERATOR} STREQUAL "Xcode")
#    set(BUILD_PLUGINS_DIR ${BUILD_DIR}/${CSOUNDLIB}.framework/Versions/${APIVERSION}/Resources/Opcodes)
#endif()
set(EXECUTABLE_INSTALL_DIR "bin")
option(USE_LIB64 "Set to on to set installation directory for libraries to lib64" OFF)
if(USE_LIB64)
    set(LIBRARY_INSTALL_DIR "lib64")
    add_definitions("-DLIB64")
else()
    set(LIBRARY_INSTALL_DIR "lib")
endif()
message(STATUS "LIBRARY_INSTALL_DIR: {LIBRARY_INSTALL_DIR}")
message(STATUS "BUILD_BIN_DIR: ${BUILD_BIN_DIR}")
message(STATUS "BUILD_LIB_DIR: ${BUILD_LIB_DIR}")
message(STATUS "BUILD_PLUGINS_DIR: ${BUILD_PLUGINS_DIR}.")
set(PLUGIN_INSTALL_DIR "${LIBRARY_INSTALL_DIR}/csound/plugins64-${APIVERSION}")
message(STATUS "LIBRARY INSTALL DIR: ${LIBRARY_INSTALL_DIR}")
message(STATUS "PLUGIN_INSTALL_DIR: ${PLUGIN_INSTALL_DIR}")
option(USE_LRINT "Use lrint/lrintf for converting floating point values to integers." ON)
option(USE_CURL "Use CURL library" ON)


# Required targets.

option(BUILD_BUCHLA_OPCODES "Build the Buchla filter opcode" ON)
option(BUILD_CHUA_OPCODES "Build the Chua opcodes" ON)
option(BUILD_DSSI_OPCODES "Build the DSSI opcodes" ON)
option(BUILD_CSOUND_VST "Build Csound as a VST plugin" ON)
option(BUILD_EMUGENS_OPCODES "Build emugens opcodes" ON)
option(BUILD_EXCITER_OPCODES "Build the exciter opcode" ON)
option(BUILD_FAUST_OPCODES "Build the Faust opcodes" ON)
option(BUILD_FLUID_OPCODES "Build the fluidsynth opcodes" ON)
option(BUILD_FRAMEBUFFER_OPCODES "Build the framebuffer opcodes" ON)
option(BUILD_IMAGE_OPCODES "Build the image opcodes" ON)
option(BUILD_JACK_OPCODES "Build the Jack related opcodes" ON)
option(BUILD_LINEAR_ALGEBRA_OPCODES "Build the Lua opcodes" ON)
option(BUILD_LUA_OPCODES "Build the Lua opcodes" ON)
option(BUILD_OSC_OPCODES "Build the OSC Opcodes" ON)
option(BUILD_P5GLOVE_OPCODES "Build the P5Glove opcodes" ON)
option(BUILD_PADSYNTH_OPCODES "Build the Padsynth opcode" ON)
option(BUILD_PLATEREV_OPCODES "Build the platerev opcode" ON)
option(BUILD_PYTHON_OPCODES "Build the Python opcodes" ON)
option(BUILD_SCANSYN_OPCODES "Build the scansyn opcodes" ON)
option(BUILD_SELECT_OPCODE "Build the select opcode" ON)
option(BUILD_SERIAL_OPCODES "Build the serial opcode" ON)
option(BUILD_STACK_OPCODES "Build the stack opcodes" ON)
option(BUILD_WEBSOCKET_OPCODE "Build the websocket opcode" ON)
option(BUILD_WIIMOTE_OPCODES "Build the Wiimote opcodes" ON)
option(BUILD_VST4CS_OPCODES "Build the vst4cs opcodes" ON)

# All dependencies are to be located from here. 
# Local TARGETS are not PACKAGES!
# Locally installed packages must be searched locally first!
# Locally built packages must be searched locally first!

# find_package(Eigen3 3.3 REQUIRED NO_MODULE)
find_package(FLTK REQUIRED NO_MODULE)
include(${FLTK_USE_FILE})
find_package(FAUST)
# find_package(fluidsynth)
find_package(Gettext)
find_package(GMM)
find_package(Java)
find_package(JNI)
# find_package(LIBLO)
find_package(LLVM)
# find_package(LUAJIT HINTS ${CMAKE_SOURCE_DIR}/dependencies/luajit-2.0})
# Just set these from our fixed local build:
set(LUA_H_PATH "${CMAKE_SOURCE_DIR}/dependencies/luajit-2.0/src")
set(LUAJIT_LIBRARY "${CMAKE_SOURCE_DIR}/dependencies/luajit-2.0/src/libluajit.so")
find_package(PORTSMF)
# find_package(portaudio2)
find_package(PNG)
find_package(PythonLibs 2.7)
find_package(Qt5 COMPONENTS Core Widgets REQUIRED PATHS "~/Qt/5.10.0/gcc_64/lib/cmake/Qt5" CONFIGS "Qt5Config.cmake")
find_package(Qt5 COMPONENTS Core Widgets REQUIRED HINTS "~/Qt/5.10.0")
find_package(STK)
find_package(SWIG)
find_package(ZLIB)

add_subdirectory(dependencies)
add_subdirectory(CsoundAC)
ExternalProject_Add(CsoundHtml5
    SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/CsoundHtml5"
    BUILD_IN_SOURCE "1"
    CONFIGURE_COMMAND "${QMAKE}"
    BUILD_COMMAND "make")
install(FILES "${CMAKE_SOURCE_DIR}/CsoundHtml5/CsoundHtml5"
    PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ
    DESTINATION "${EXECUTABLE_INSTALL_DIR}")
add_subdirectory(CsoundVst)

set (CPACK_PACKAGE_NAME "csound-extended")
set (CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/COPYING")
set (CPACK_PACKAGE_VERSION "6.11.0-alpha")
set (CPACK_PACKAGE_VERSION_MAJOR "6")
set (CPACK_PACKAGE_VERSION_MINOR "11")
set (CPACK_PACKAGE_VERSION_PATCH "0")
include (CPack)
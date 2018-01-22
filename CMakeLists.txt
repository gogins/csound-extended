cmake_minimum_required(VERSION 3.5)

message("CMAKE BUILD SYSTEM FOR CSOUND-EXTENDED")
message("Execute \"cd dependencies;./update-dependencies.sh;cd..\" before build!")
message("Repeat only when dependencies change.")
message("There are NO configuration options in this build.")
message("But if there ARE configuration variables, set them here.")

# A certain amount of the following has been copied from the core Csound project,
# and hoisted up here to enable paths etc. to be set for all projects here.

project(csound-extended)

include(ExternalProject)
set(CMAKE_PREFIX_PATH "${CMAKE_SOURCE_DIR}/local-linux")
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules")
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/dependencies/csound/cmake/Modules")
message(STATUS "CMAKE_MODULE_PATH: ${CMAKE_MODULE_PATH}")
set(CMAKE_BUILD_TYPE "RelWithDebInfo")
set(ALLOW_DUPLICATE_CUSTOM_TARGETS "Yes")

set(USE_DOUBLE "Yes")
set(BUILD_CSOUND_VST "On")
set(BUILD_LINEAR_ALGEBRA_OPCODES "On")
set(QMAKE "/home/mkg/Qt/5.10.0/gcc_64/bin/qmake")
set(QT_INSTALL_PREFIX "~/Qt/5.10.0/gcc_64")
set(CSOUND_INCLUDE_PATH "${CMAKE_SOURCE_DIR}/dependencies/csound/include")
message(STATUS "CSOUND_INCLUDE_PATH: ${CSOUND_INCLUDE_PATH}")
set(VSTSDK2X_INCLUDE_DIR "${CMAKE_SOURCE_DIR}/dependencies/VST_SDK/VST2_SDK")
message(STATUS "VSTSDK2X_INCLUDE_DIR: ${VSTSDK2X_INCLUDE_DIR}")
set(BUILD_DIR ${CMAKE_CURRENT_BINARY_DIR})
#if(USE_DOUBLE)
#    set(BUILD_PLUGINS_DIR ${BUILD_DIR}/plugins64)
#else()
#    set(BUILD_PLUGINS_DIR ${BUILD_DIR}/plugins)
#endif()
set(BUILD_PLUGINS_DIR ${BUILD_DIR})
set(BUILD_BIN_DIR ${BUILD_DIR})
set(BUILD_LIB_DIR ${BUILD_DIR})

#if(APPLE AND NOT ${CMAKE_GENERATOR} STREQUAL "Xcode")
#    set(BUILD_PLUGINS_DIR ${BUILD_DIR}/${CSOUNDLIB}.framework/Versions/${APIVERSION}/Resources/Opcodes)
#endif()

message(STATUS "BUILD_BIN_DIR set to ${BUILD_BIN_DIR}.")
message(STATUS "BUILD_LIB_DIR set to ${BUILD_LIB_DIR}.")
message(STATUS "BUILD_PLUGINS_DIR set to ${BUILD_PLUGINS_DIR}.")

option(USE_LIB64 "Set to on to set installation directory for libraries to lib64" ON)
if(USE_LIB64)
    set(LIBRARY_INSTALL_DIR "lib64")
    add_definitions("-DLIB64")
else()
    set(LIBRARY_INSTALL_DIR "lib")
endif()
set(PLUGIN_INSTALL_DIR "${LIBRARY_INSTALL_DIR}/csound/plugins64-${APIVERSION}")
message(STATUS "LIBRARY INSTALL DIR: ${LIBRARY_INSTALL_DIR}")
message(STATUS "PLUGIN_INSTALL_DIR: ${PLUGIN_INSTALL_DIR}")

# All dependencies are to be located from here. 
# Local TARGETS are not really PACKAGES!
# Locally installed packages must be searched locally first!
# Locally built packages must be searched locally first!

find_package(FLTK CONFIG REQUIRED)
# find_package(Eigen3 3.3 REQUIRED NO_MODULE)
# find_package(fluidsynth)
find_package(FAUST)
find_package(Gettext)
# find_package(GMM)
find_package(Java)
find_package(JNI)
# find_package(LIBLO)
find_package(LLVM)
# find_package(luajit)
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
add_subdirectory(CsoundVst)

#add_dependencies("CsoundAC" "csound")
#add_dependencies("CsoundHtml5" "csound")
#add_dependencies("csound.node" "csound")
#add_dependencies("csoundvstmain" "csound")

cmake_minimum_required(VERSION 3.5)

message("CMAKE BUILD SYSTEM FOR CSOUND-EXTENDED")
message("For a fresh build, run 'bash fresh-build-linux.sh'")
#message("To update all dependencies, run cmake with '-DUPDATE_DEPENDENCIES'")
#if(UPDATE_DEPENDENCIES)
#    message("Updating all dependencies...")
#    execute_process(COMMAND "pwd" WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}/dependencies")
#    execute_process(COMMAND "${CMAKE_SOURCE_DIR}/dependencies/update-dependency-downloads.sh" WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}/dependencies")
#    execute_process(COMMAND "${CMAKE_SOURCE_DIR}/dependencies/update-dependency-packages.sh" WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}/dependencies")
#    execute_process(COMMAND "${CMAKE_SOURCE_DIR}/dependencies/update-dependency-submoduless.sh" WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}/dependencies")
#    execute_process(COMMAND "${CMAKE_SOURCE_DIR}/dependencies/update-dependency-reppositories.sh" WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}/dependencies")
#    execute_process(COMMAND "${CMAKE_SOURCE_DIR}/dependencies/patch-dependency-sources.sh" WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}/dependencies")
#endif()
message("There are NO configuration options in this build.")
message("But if there ARE configuration variables, set them in the ")
message(" optional \"custom-variables.cmake\" file in this directory.")
include("custom-variables.cmake" OPTIONAL RESULT_VARIABLE CSOUND_EXTENDED_CUSTOM_CONFIGURATION)
message("CSOUND_EXTENDED_CUSTOM_CONFIGURATION: ${CSOUND_EXTENDED_CUSTOM_CONFIGURATION}")

project(csound-extended)

# A certain amount of the following has been copied from the core Csound project,
# and hoisted up here to enable paths etc. to be set for all projects here.

# Top-level CMake configuration.

include(ExternalProject)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}  -Wall")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall")
set(CPACK_ARCHIVE_COMPONENT_INSTALL ON)
set(CMAKE_INSTALL_DEFAULT_COMPONENT_NAME "csound-base")
#add_definitions(-DGMM_VERSION=513)
add_definitions(-Dlinux)

# This is used for both Csound and CsoundAC SONAMEs.
set(APIVERSION "6.0")
set(CMAKE_PREFIX_PATH "${CMAKE_SOURCE_DIR}/local-linux")
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules")
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/dependencies/csound/cmake/Modules")
message(STATUS "CMAKE_MODULE_PATH: ${CMAKE_MODULE_PATH}")
set(CMAKE_BUILD_TYPE "RelWithDebInfo")
set(ALLOW_DUPLICATE_CUSTOM_TARGETS "Yes")
add_definitions(-DUSE_DOUBLE)
set(USE_DOUBLE "Yes")
configure_file(${CMAKE_SOURCE_DIR}/dependencies/csound/include/float-version.h.in ${CMAKE_CURRENT_BINARY_DIR}/float-version.h)
set(QT_INSTALL_PREFIX "/usr")
# If you have a local installation of the Qt SDK, set an environment variable 
# QMAKE with the pathname to your local qmake.
if(DEFINED ENV{QMAKE})
    set(QMAKE $ENV{QMAKE})
else()
    set(QMAKE "qmake")
endif()
message("Using qmake: ${QMAKE}.")
if(DEFINED ENV{Qt5_DIR})
    set(Qt5_DIR $ENV{Qt5_DIR})
    message("Using Qt5_DIR: ${Qt5_DIR}.")
endif()
message("Using qmake: ${QMAKE}.")
set(CSOUND_INCLUDE_PATH "${CMAKE_SOURCE_DIR}/dependencies/csound/include")
message(STATUS "CSOUND_INCLUDE_PATH: ${CSOUND_INCLUDE_PATH}")
set(VSTSDK2X_INCLUDE_DIR "${CMAKE_SOURCE_DIR}/dependencies/VST_SDK/VST2_SDK")
message(STATUS "VSTSDK2X_INCLUDE_DIR: ${VSTSDK2X_INCLUDE_DIR}")
set(BUILD_DIR ${CMAKE_CURRENT_BINARY_DIR})
set(BUILD_PLUGINS_DIR ${BUILD_DIR})
set(BUILD_BIN_DIR ${BUILD_DIR})
set(BUILD_LIB_DIR ${BUILD_DIR})
set(EXECUTABLE_INSTALL_DIR "bin")
option(USE_LIB64 "Set to on to set installation directory for libraries to lib64" OFF)
if(USE_LIB64)
    set(LIBRARY_INSTALL_DIR "lib64")
    add_definitions("-DLIB64")
else()
    set(LIBRARY_INSTALL_DIR "lib")
endif()
message(STATUS "CMAKE_INSTALL_PREFIX: ${CMAKE_INSTALL_PREFIX}")
message(STATUS "CPACK_PACKAGING_INSTALL_PREFIX: ${CPACK_PACKAGING_INSTALL_PREFIX}")
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
option(BUILD_STATIC_LIBRARY "Build the static Csound library" ON)
option(BUILD_WEBSOCKET_OPCODE "Build the websocket opcode" ON)
option(BUILD_WIIMOTE_OPCODES "Build the Wiimote opcodes" ON)

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
find_package(portaudio2)
find_package(PNG)
find_package(PythonLibs 2.7)
find_package(Qt5 COMPONENTS Core Widgets REQUIRED PATHS "~/Qt/5.10.0/gcc_64/lib/cmake/Qt5" CONFIGS "Qt5Config.cmake")
find_package(Qt5 COMPONENTS Core Widgets REQUIRED HINTS "~/Qt/5.10.0")
find_package(STK)
find_package(SWIG)
find_package(ZLIB)

# Copied from dependencies/csound/CMakeLists.txt:

# Utility function to make plugins. All plugin targets should use this as it
# sets up output directory set in top-level CmakeLists.txt
# and adds the appropriate install target
#
# libname - name of library to produce
# srcs - list of src files (must be quoted if a list)
# extralibs (OPTIONAL) - extra libraries to link the plugin to
#
# NB - this was moved here as it needs some VARS defined above
# for setting up the framework
function(make_plugin libname srcs)
    if(APPLE)
        add_library(${libname} SHARED ${srcs})
    else()
        add_library(${libname} MODULE ${srcs})
    endif()
    set(i 2)
    while( ${i} LESS ${ARGC} )
        if(NOT MSVC OR NOT("${ARGV${i}}" MATCHES "m"))
            target_link_libraries(${libname} ${ARGV${i}})
        endif()
        math(EXPR i "${i}+1")
    endwhile()
    set_target_properties(${libname} PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY ${BUILD_PLUGINS_DIR}
        LIBRARY_OUTPUT_DIRECTORY ${BUILD_PLUGINS_DIR}
        ARCHIVE_OUTPUT_DIRECTORY ${BUILD_PLUGINS_DIR})
    install(TARGETS ${libname}
	LIBRARY DESTINATION "${PLUGIN_INSTALL_DIR}" )
endfunction(make_plugin)

add_custom_target(doxygen_reference
    ALL 
    COMMAND doxygen
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/doc")

add_subdirectory(dependencies)
add_subdirectory(Opcodes/cmask)
add_subdirectory(CsoundAC)
ExternalProject_Add(CsoundHtml5
    SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/CsoundHtml5"
    BUILD_IN_SOURCE "1"
    CONFIGURE_COMMAND "${QMAKE}"
    BUILD_COMMAND "make")
install(FILES "${CMAKE_SOURCE_DIR}/CsoundHtml5/CsoundHtml5"
    PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ
    DESTINATION "${EXECUTABLE_INSTALL_DIR}"
    COMPONENT csound-html5)
ExternalProject_Add(CsoundNode
    SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/csound.node"
    BUILD_IN_SOURCE "1"
    CONFIGURE_COMMAND ""
    BUILD_COMMAND nw-gyp rebuild --target=0.30.2
    INSTALL_COMMAND "")
install(FILES "${CMAKE_SOURCE_DIR}/csound.node/build/Release/csound.node"
    PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ
    DESTINATION "${EXECUTABLE_INSTALL_DIR}"
    COMPONENT csound-node)

# These are the real targets: the packages.

install(FILES ${CMAKE_SOURCE_DIR}/copyright.txt
        DESTINATION share/doc/csound-extended
        RENAME copyright)
install(DIRECTORY ${CMAKE_SOURCE_DIR}/doc/html
        DESTINATION share/doc/csound-extended)

set(CPACK_GENERATOR TGZ; DEB)
set(CPACK_DEBIAN_PACKAGE_MAINTAINER "Michael Gogins <michael.gogins@gmail.com>")
set(CPACK_DEBIAN_PACKAGE_DESCRIPTION "Extends Csound with CsoundHtml5, CsoundAC, and others.")
set(CPACK_DEBIAN_PACKAGE_SHLIBDEPS ON)
set(CPACK_PACKAGE_NAME "csound-extended")
set(CPACK_PACKAGE_CONTACT "csound-devel@listserv.heanet.ie")
set(CPACK_PACKAGE_VERSION "$ENV{CSOUND_EXTENDED_VERSION}")
set(CPACK_STRIP_FILES "YES")
set(CPACK_DEB_COMPONENT_INSTALL "ON")   
set(CPACK_DEBIAN_PACKAGE_DEPENDS "")
set(CPACK_DEBIAN_PACKAGE_HOMEPAGE "https://github.com/gogins/csound-extended")
set(CPACK_DEBIAN_PACKAGE_SHLIBDEPS "ON")
set(CPACK_COMPONENTS_ALL csound-base csound-html5 csound-node csound-cmask)
set(CPACK_COMPONENTS_ALL_IN_ONE_PACKAGE 1)

include (CPack)

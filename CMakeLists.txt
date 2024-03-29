cmake_minimum_required(VERSION 3.5)
project(csound-extended)

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
message(" optional \"Custom.cmake\" file in this directory.")
include("Custom.cmake" OPTIONAL RESULT_VARIABLE CSOUND_EXTENDED_CUSTOM_CONFIGURATION)
message("CSOUND_EXTENDED_CUSTOM_CONFIGURATION: ${CSOUND_EXTENDED_CUSTOM_CONFIGURATION}")

# A certain amount of the following has been copied from the core Csound project,
# and hoisted up here to enable paths etc. to be set for all projects here.

# Top-level CMake configuration.

include(ExternalProject)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC -Wall -Wno-sign-compare -Wno-unknown-pragmas -Wno-misleading-indentation")
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
option(BUILD_FLUID_OPCODES "Build the fluidsynth opcodes" ON)
option(BUILD_FRAMEBUFFER_OPCODES "Build the framebuffer opcodes" ON)
option(BUILD_IMAGE_OPCODES "Build the image opcodes" ON)
option(BUILD_JACK_OPCODES "Build the Jack related opcodes" ON)
option(BUILD_LINEAR_ALGEBRA_OPCODES "Build the linear algebra opcodes" ON)
option(BUILD_LUA_OPCODES "Build the Lua opcodes" OFF)
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

# First we locate all system packages, these are used to make the Ubuntu
# csound-extended package, even though local packages may be used for other
# targets.
message("CMAKE_SYSTEM_NAME: ${CMAKE_SYSTEM_NAME}")
set(CMAKE_PREFIX_PATH /usr/local)
find_package(Csound REQUIRED)
message("CSOUND_LIBRARIES: ${CSOUND_LIBRARIES}")
find_library(LIBSNDFILE_LIBRARY sndfile REQUIRED)
find_library(CSND6_LIBRARY csnd6 REQUIRED)
find_package(Eigen3 REQUIRED CONFIG)
# checking property of the target
get_target_property(inc_dir Eigen3::Eigen INTERFACE_INCLUDE_DIRECTORIES)
message("[DEBUG] inc_dir: ${inc_dir}")
# or checking the Eigen variable
message("[DEBUG] EIGEN3_INCLUDE_DIRS: ${EIGEN3_INCLUDE_DIRS}")
find_package(MUSICXML)
find_package(OpenCV REQUIRED)
find_package(Boost REQUIRED)
message("[DEBUG] Boost_INCLUDE_DIR: ${Boost_INCLUDE_DIR}")
message("[DEBUG] Boost_COMPONENTS: ${Boost_COMPONENTS}")
find_package(PORTSMF)
find_package(Python3 COMPONENTS Interpreter Development)
find_package(SWIG)
include(UseSWIG)
find_package(SWIG)

include_directories("${CSOUND_INCLUDE_DIR}")
include_directories("${CSOUND_INCLUDE_DIR}/csound")
include_directories("${CSOUND_SOURCE_HOME}/interfaces")
if (MUSICXML_FOUND)
    include_directories("${MUSICXML_INCLUDE_DIRS}")
endif()

# Copied from dependencies/csound/CMakeLists.txt:

# Checks if dependencies for an enabled target are fulfilled.
# If FAIL_MISSING is true and the dependencies are not fulfilled,
# it will abort the cmake run.
# If FAIL_MISSING is false, it will set the option to OFF.
# If the target is not enabled, it will do nothing.
# example: check_deps(BUILD_NEW_PARSER FLEX_EXECUTABLE BISON_EXECUTABLE)
function(check_deps option)
    if(${option})
        set(i 1)
        while( ${i} LESS ${ARGC} )
            set(dep ${ARGV${i}})
            if(NOT ${dep})
                if(FAIL_MISSING)
                    message(FATAL_ERROR
                        "${option} is enabled, but ${dep}=\"${${dep}}\"")
                else()
                    message(STATUS "${dep}=\"${${dep}}\", so disabling ${option}")
                    set(${option} OFF PARENT_SCOPE)
                    # Set it in the local scope too
                    set(${option} OFF)
                endif()
            endif()
            math(EXPR i "${i}+1")
        endwhile()
    endif()
    if(${option})
        message(STATUS "${option} is enabled.")
    else()
        message(STATUS "${option} is disabled.")
    endif()
endfunction(check_deps)

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

# If we're not building Csound locally, we don't need these dependencies.
# add_subdirectory(dependencies)

add_subdirectory(Opcodes/cmask)
add_subdirectory(Opcodes/MVerb)
add_subdirectory(CsoundAC)

# These are the real targets: the packages.

# distro codename
# find stuff we need
find_program(LSB_RELEASE_CMD lsb_release)
if(NOT DISTRO_CODENAME)
  if(NOT LSB_RELEASE_CMD)
    message(WARNING "DEB Generator: Can't find lsb_release in your path. Setting DISTRO_CODENAME to unknown.")
    set(DISTRO_CODENAME unknown)
  else()
    execute_process(COMMAND ${LSB_RELEASE_CMD} -cs
                    OUTPUT_VARIABLE DISTRO_CODENAME
                    OUTPUT_STRIP_TRAILING_WHITESPACE)
  endif()
endif()

set(CPACK_COMPONENTS_ALL csound-base csound-cmask csound-mverb)
set(CPACK_COMPONENTS_ALL_IN_ONE_PACKAGE "OFF")
set(CPACK_GENERATOR TGZ; DEB)
set(CPACK_PACKAGE_CONTACT "${CPACK_DEBIAN_PACKAGE_MAINTAINER}")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_SOURCE_DIR}/README.md")
set(CPACK_PACKAGE_NAME "csound-extended-dev")
set(CPACK_PACKAGE_VERSION "2.3.0")
# set(CPACK_STRIP_FILES "YES")

set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/copyright.txt")
set(CPACK_RESOURCE_FILE_README "${CMAKE_SOURCE_DIR}/README.md")

set(CPACK_DEBIAN_ARCHITECTURE ${CMAKE_SYSTEM_PROCESSOR})
set(CPACK_DEBIAN_COMPONENT_INSTALL "ON")
# set(CPACK_DEBIAN_PACKAGE_CONTROL_EXTRA "${CMAKE_SOURCE_DIR}/DEBIAN/triggers" "${CMAKE_SOURCE_DIR}/DEBIAN/postrm" "${CMAKE_SOURCE_DIR}/DEBIAN/postinst")
set(CPACK_DEBIAN_PACKAGE_CONTROL_EXTRA "${CMAKE_SOURCE_DIR}/DEBIAN/triggers")
set(CPACK_DEBIAN_PACKAGE_CONFLICTS "libcsoundac-dev, libcsoundac6.0v5")
set(CPACK_DEBIAN_PACKAGE_DEPENDS "python3")
set(CPACK_DEBIAN_PACKAGE_DESCRIPTION "Extends the sound programming language
 and library Csound. Additional features include the CsoundAC library for
 algorithmic composition, the cmask opcode for algorithmic composition
 and granular synthesis, and others.")
set(CPACK_DEBIAN_PACKAGE_ENHANCES "csound, libsound64-dev, csound-python, csoundqt")
set(CPACK_DEBIAN_PACKAGE_HOMEPAGE "https://github.com/gogins/csound-extended")
set(CPACK_DEBIAN_PACKAGE_LICENSE "${CMAKE_SOURCE_DIR}/copyright.txt")
set(CPACK_DEBIAN_PACKAGE_MAINTAINER "Michael Gogins <michael.gogins@gmail.com>")
set(CPACK_DEBIAN_PACKAGE_NAME "${CPACK_PACKAGE_NAME}")
set(CPACK_DEBIAN_PACKAGE_PRIORITY "optional")
set(CPACK_DEBIAN_PACKAGE_VERSION "${CPACK_PACKAGE_VERSION}")
# For your first build on your system, set this to "OFF", build, and install.
# Then, set this to "ON", rebuild, and reinstall. This is a workaround for a 
# bug in how CPack interacts with shlibdeps.
set(CPACK_DEBIAN_PACKAGE_SHLIBDEPS "ON")
set(CPACK_DEBIAN_PACKAGE_GENERATE_SHLIBS "ON")
set(CPACK_DEBIAN_PACKAGE_GENERATE_SHLIBS_POLICY ">=")
# generate a Debian compliant changelog
string(TIMESTAMP RFC2822_TIMESTAMP "%a, %d %b %Y %H:%M:%S -0400" UTC)
set(CHANGELOG_HEADER "csound-extended-dev (${CPACK_PACKAGE_VERSION}) ${DISTRO_CODENAME}\; urgency=high")
# two spaces between maintainer and timestamp is NOT a mistake
set(CHANGELOG_FOOTER " -- ${CPACK_DEBIAN_PACKAGE_MAINTAINER}  ${RFC2822_TIMESTAMP}")
execute_process(COMMAND bash ${CMAKE_SOURCE_DIR}/git-changelog.sh
    OUTPUT_VARIABLE CHANGELOG
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    OUTPUT_STRIP_TRAILING_WHITESPACE)
# message("CHANGELOG:\n${CHANGELOG}")
string(REPLACE "\"" "" CHANGELOG ${CHANGELOG})
#file(WRITE ${CMAKE_SOURCE_DIR}/debian/changelog.Debian ${CHANGELOG_HEADER}\n${CHANGELOG}\n\n${CHANGELOG_FOOTER})
#execute_process(COMMAND gzip -f -9 -n ${CMAKE_SOURCE_DIR}/debian/changelog.Debian)
file(WRITE ${CMAKE_SOURCE_DIR}/debian/changelog ${CHANGELOG_HEADER}\n${CHANGELOG}\n\n${CHANGELOG_FOOTER})
execute_process(COMMAND gzip -f -9 -n ${CMAKE_SOURCE_DIR}/debian/changelog)
unset(CHANGELOG_HEADER)
unset(CHANGELOG_FOOTER)
unset(RFC2822_TIMESTAMP)

install(FILES ${CMAKE_SOURCE_DIR}/copyright.txt
    DESTINATION share/doc/${CPACK_DEBIAN_PACKAGE_NAME}
    RENAME copyright)
install(DIRECTORY ${CMAKE_SOURCE_DIR}/doc/html
    DESTINATION share/doc/${CPACK_DEBIAN_PACKAGE_NAME})
install(FILES "${CMAKE_SOURCE_DIR}/debian/changelog.gz"
    DESTINATION "share/doc/${CPACK_DEBIAN_PACKAGE_NAME}")
install(FILES ${CMAKE_SOURCE_DIR}/cmake/cpack/deb/lintian/overrides/csound-extended-dev
    DESTINATION "share/lintian/overrides/"
    COMPONENT csound-extended-dev)

# And all the README.md files, renamed to not conflict...

install(FILES "README.md"
    DESTINATION share/doc/${CPACK_DEBIAN_PACKAGE_NAME})
install(FILES "REALTIME-AUDIO.md"
    DESTINATION share/doc/${CPACK_DEBIAN_PACKAGE_NAME})
install(FILES "silencio/patches/README.md"
    DESTINATION share/doc/${CPACK_DEBIAN_PACKAGE_NAME}
    RENAME "README-CSOUND-PATCHES.md")
install(FILES "silencio/README.md"
    DESTINATION share/doc/${CPACK_DEBIAN_PACKAGE_NAME}
    RENAME "README-SILENCIO.md")

include (CPack)

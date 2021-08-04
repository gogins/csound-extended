cmake_minimum_required(VERSION 3.5)

message("CMAKE BUILD SYSTEM FOR THE CSOUND PLAYPEN")

project(csound-playpen)

# Top-level CMake configuration.

cmake_policy(SET CMP0078 NEW)
cmake_policy(SET CMP0086 NEW)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules")

find_package(PkgConfig REQUIRED)
pkg_check_modules(WEBKIT2_GTK_WEB_EXTENSION REQUIRED webkit2gtk-web-extension-4.0)

# This is here only to warn in advance that the sourceview component will be 
# missing when running the playpen.

pkg_check_modules(GTKSOURCEVIEW REQUIRED gtksourceview-4)
find_package(Csound REQUIRED)
find_package(Python3 REQUIRED COMPONENTS Interpreter Development)
find_package(SWIG)
include(UseSWIG)
include_directories(${Python3_INCLUDE_DIRS})
include_directories(${CSOUND_INCLUDE_DIRS})
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wno-sign-compare -Wno-unknown-pragmas -Wno-misleading-indentation -Wno-unused-variable")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall")
add_definitions("-DUSE_DOUBLE")
if(USE_LIB64)
    set(LIBRARY_INSTALL_DIR "lib64")
    add_definitions("-DLIB64")
else()
    set(LIBRARY_INSTALL_DIR "lib")
endif()

set_property(SOURCE csound.i PROPERTY CPLUSPLUS ON)
swig_add_library(CsoundThreaded
                 TYPE MODULE
                 LANGUAGE python
                 SOURCES csound.i
                )
swig_link_libraries(CsoundThreaded ${Python3_LIBRARY_RELEASE} ${CSOUND_LIBRARY} ${LIBSNDFILE_LIBRARY})
install(TARGETS CsoundThreaded
    LIBRARY DESTINATION "${Python3_SITEARCH}"
    ARCHIVE DESTINATION "${Python3_SITEARCH}")
install(FILES "CsoundThreaded.py"
    DESTINATION "${Python3_SITEARCH}")

add_library(jsc_csound SHARED
    jsc_csound.cpp)
target_include_directories(jsc_csound PUBLIC ${WEBKIT2_GTK_WEB_EXTENSION_INCLUDE_DIRS})
target_link_libraries(jsc_csound PUBLIC ${WEBKIT2_GTK_WEB_EXTENSION_LIBRARIES} ${CSOUND_LIBRARY} ${LIBSNDFILE_LIBRARY})  
install(TARGETS jsc_csound
        LIBRARY DESTINATION "${LIBRARY_INSTALL_DIR}")

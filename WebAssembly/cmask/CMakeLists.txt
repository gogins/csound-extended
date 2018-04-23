
#    Copyright (C) 2018 Michael Gogins#
#
#    This file is part of csound-extended.
#
#    The Csound Library is free software; you can redistribute it
#    and/or modify it under the terms of the GNU Lesser General Public
#    License as published by the Free Software Foundation; either
#    version 2.1 of the License, or (at your option) any later version.
#
#    Csound is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU Lesser General Public License for more details.
#
#    You should have received a copy of the GNU Lesser General Public
#    License along with Csound; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
#    02111-1307 USA
#
#    As a special exception, if other files instantiate templates or
#    use macros or inline functions from this file, this file does not
#    by itself cause the resulting executable or library to be covered
#    by the GNU Lesser General Public License. This exception does not
#    however invalidate any other reasons why the library or executable
#    file might be covered by the GNU Lesser General Public License.

cmake_minimum_required(VERSION 3.7)
project(CMask)
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_FLAGS "-std=c++11")
set(CMAKE_C_COMPILER "emcc")
set(CMAKE_CXX_COMPILER "em++")
set(CMASK_SOURCE_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../../dependencies/cmask/source")
add_library(cmask cmask_library.cpp 
${CMASK_SOURCE_DIRECTORY}/cmask.cpp 
${CMASK_SOURCE_DIRECTORY}/event.cpp 
${CMASK_SOURCE_DIRECTORY}/field.cpp 
${CMASK_SOURCE_DIRECTORY}/fileio.cpp 
${CMASK_SOURCE_DIRECTORY}/gen.cpp 
${CMASK_SOURCE_DIRECTORY}/items.cpp 
${CMASK_SOURCE_DIRECTORY}/mask.cpp 
${CMASK_SOURCE_DIRECTORY}/parser.cpp 
${CMASK_SOURCE_DIRECTORY}/quant.cpp 
${CMASK_SOURCE_DIRECTORY}/tables.cpp 
${CMASK_SOURCE_DIRECTORY}/utils.cpp)
target_include_directories(cmask PRIVATE "${CMASK_SOURCE_DIRECTORY}")
set(EMCC_LINKER_FLAGS "-s SAFE_HEAP=1 --bind -s WASM=1 -O3 -o ../index.js -s ASSERTIONS=1 -s TOTAL_MEMORY=268435456 -s ALLOW_MEMORY_GROWTH=1 -s INVOKE_RUN=0  -std=c++11 --bind")
set(CMAKE_REQUIRED_FLAGS "${EMCC_LINKER_FLAGS}")
set(CMAKE_EXE_LINKER_FLAGS "${EMCC_LINKER_FLAGS}")


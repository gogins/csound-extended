
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
set(CMAKE_CXX_STANDARD 11)
set(CMASK_SOURCE_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../../dependencies/cmask/source")
set(CMASK_OPCODE_SOURCES cmask_opcodes.cpp)
make_plugin(cmask_opcodes ${CMASK_OPCODE_SOURCES})
set_target_properties(cmask_opcodes PROPERTIES COMPILE_FLAGS "-fPIC -std=c++11 -Wno-multichar")
target_include_directories(cmask_opcodes PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../../dependencies/csound/include ${CMASK_SOURCE_DIRECTORY})

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

cmake_minimum_required(VERSION 3.5)
set(CMAKE_CXX_STANDARD 11)
if(NOT APPLE)
  check_deps(BUILD_LUA_OPCODES LUAJIT_LIBRARY LUAJIT_INCLUDE_DIRS)
  if(BUILD_LUA_OPCODES)
    make_plugin(lua_opcodes LuaCsound.cpp ${LUAJIT_LIBRARY})
    target_compile_options(lua_opcodes PRIVATE ${OpenMP_CXX_FLAGS})
    target_include_directories(lua_opcodes PRIVATE ${LUAJIT_INCLUDE_DIRS})
    install(TARGETS lua_opcodes
        LIBRARY DESTINATION "${PLUGIN_INSTALL_DIR}"
        PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ
        COMPONENT csound-luajit)
  endif()
endif()

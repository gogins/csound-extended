/*

    Copyright (C) 2018 Michael Gogins

    This file is part of csound-extended.

    The Csound Library is free software; you can redistribute it
    and/or modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    Csound is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with Csound; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
    02111-1307 USA

    As a special exception, if other files instantiate templates or
    use macros or inline functions from this file, this file does not
    by itself cause the resulting executable or library to be covered
    by the GNU Lesser General Public License. This exception does not
    however invalidate any other reasons why the library or executable
    file might be covered by the GNU Lesser General Public License.
*/
#include <cstdio>
#include <eigen3/Eigen/Dense>
#include <emscripten/bind.h>
#include <fstream>
#include <iostream>
#include <CsoundAC/Silence.hpp>
#include <string>
#include <sstream>

static void csoundac() {
    csound::System::message("Hello, this is CsoundAC.\n");
}

EMSCRIPTEN_BINDINGS(csoundac) {         
    emscripten::function("csoundac", &csoundac);
    emscripten::class_<Eigen::MatrixXd>("MatrixXd")
        .constructor<>()
        .constructor<int, int>()
    ;
    emscripten::class_<csound::Node>("Node")
        .constructor<>()
    ;
}
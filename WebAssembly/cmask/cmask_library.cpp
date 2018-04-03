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
#include <emscripten/bind.h>
#include <fstream>
#include <iostream>
#include <string>
#include <sstream>

#include "event.h"
#include "mask.h"
#include "utils.h"
#include "fileio.h"
#include "parser.h"


/** 
 * Takes the text of a CMask parameter file, and  
 * returns the text of the generated Csound score file.
 */
std::string cmask(const std::string &parameters_text) {
    static bool initialized = false;
    if (!initialized) {
        frandinit();
        initialized = true;
    }
    //std::cout << "CMask parameters: " << std::endl << parameters_text << std::endl;
    // Rather than monkey with the existing code,
    // we just use temporary files.
    char parameters_filename[0x100];
    std::tmpnam(parameters_filename);
    std::string score_filename = parameters_filename;
    score_filename.append(".sco");
    std::ofstream parameters_file(parameters_filename);
    parameters_file << parameters_text << std::endl;
    parameters_file.close();
    static scanner scanner_;
	scanner_.scn(parameters_filename, (char *)score_filename.c_str());
    scanner_.analyze(); 
    std::ifstream score_file(score_filename);
    std::stringstream score_buffer;
    score_buffer << score_file.rdbuf();
    score_file.close();
    std::string score_text = score_buffer.str();
    //std::cout << "CMask generated: " << std::endl << score_text << std::endl;
    std::remove(parameters_filename);
    std::remove(score_filename.c_str());
    return score_text;
}

EMSCRIPTEN_BINDINGS(cmask) {         
   emscripten::function("cmask", &cmask);
}
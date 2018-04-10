/*
    cmask_opcodes.cpp

    Copyright (C) 2018 Michael Gogins

    This source code is for CMask opcodes in Csound.
    The CMask opcodes are licensed under the terms of 
    the Mozilla Public License, version 2.0.
    
 */
#include <OpcodeBase.hpp>
#include <cerrno>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <string>
#include <sstream>

// Because CMask sources have symbols whose names conflict with Csound (e.g.
// "GEN", and because I do not want to extensively change CMask sources, 
// we simply include all CMask sources into this one compilation unit, but in 
// a separate namespace.

namespace cmask {
using namespace std;
#include "globals.h"
#include "event.h"
#include "field.h"
#include "fileio.h"
#include "gen.h"
#include "items.h"
#include "mask.h"
#include "parser.h"
#include "quant.h"
#include "tables.h"
#include "utils.h"
#include "cmask.cpp"
#include "event.cpp"
#include "field.cpp"
#include "fileio.cpp"
#include "gen.cpp"
#include "items.cpp"
#include "mask.cpp"
#include "parser.cpp"
#include "quant.cpp"
#include "tables.cpp"
#include "utils.cpp"
};

/*! \mainpage cmask 
 * <h2>Description</h2>
 *
 * Encapsulates all functionality of Andre Bartetzki's CMask program 
 * for algorithmic composition into one i-rate Csound opcode.
 * THe cmask opcode receives a string containing the text of a CMask 
 * parameter file, and returns a string containing the generated Csound score.
 * If the optional play_immediately parameter is true, the generated score is 
 * immediately played. See <a href="http://www.bartetzki.de/en/software.html">this reference and article.</a>
 * 
 * <h2>Syntax</h2>
 *
 * <code>iscore_text <b>cmask</b> iparameters_text [, iplay_immediately]</code>
 *
 * <h2>Initialization</h2>
 *
 * <i>iparameters_text</i> -- A string, usually multi-line (delimited <code>{{</code> and <code>}}</code>, that 
 * contains any text that might be found in a CMask parameters file.
 *
 * <i>iplay_immediately</i> -- If 1, the score events generated from the parameters are immediately scheduled 
 * for performance. The default is 1.
 *
 * <h2>Performance</h2>
 *
 * <i>iscore_text</i> -- The text of all score events generated from the parameters, in case the user wishes to 
 * further process these events before performing them.
 *
 * <h2>Credits</h2>
 *
 * Andre Bartetzki<br>
 * <a href="http://www.bartetzki.de">http://www.bartetzki.de</a><br>
 * 1997
 *
 * Michael Gogins<br>
 * <a href="http://michaelgogins.tumblr.dom">http://michaelgogins.tumblr.com</a><br>
 * 2018
 *
 */
class CMask : public csound::OpcodeBase<CMask>
{
public:
  // Csound opcode outputs.
  STRINGDAT *score;
  // Csound opcode inputs.
  STRINGDAT *parameters_text;
  MYFLT *play_immediately;
  int init(CSOUND *csound)
  {
    cmask::frandinit();
    cmask::scanner scanner_;
    //std::cout << "CMask parameters: " << std::endl << parameters_text << std::endl;
    // Rather than monkey with the existing code,
    // we just use temporary files.
    char parameters_filename[0x100];
    std::tmpnam(parameters_filename);
    std::string score_filename = parameters_filename;
    score_filename.append(".sco");
    std::ofstream parameters_file(parameters_filename);
    parameters_file << parameters_text->data << std::endl;
    parameters_file.close();
    scanner_.scn(parameters_filename, const_cast<char *>(score_filename.c_str()));
    scanner_.analyze(); 
    std::ifstream score_file(score_filename);
    std::stringstream score_buffer;
    score_buffer << score_file.rdbuf();
    score_file.close();
    std::string score_text = score_buffer.str();
    //std::cout << "CMask generated: " << std::endl << score_text << std::endl;
    std::remove(parameters_filename);
    std::remove(score_filename.c_str());
    score->data = csound->Strdup(csound, const_cast<char *>(score_text.c_str()));
    score->size = score_text.size() + 1;
    if (*play_immediately) {
        csound->InputMessage(csound, score->data);
    }
    return OK;
  }
};

extern "C"
{
  OENTRY oentries[] =
    {
      {
        (char*)"cmask",
        sizeof(CMask),
        0,
        1,
        (char*)"S",
        (char*)"Sp",
        (SUBR) CMask::init_,
        0,
        0,
      },
      {
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
      }
    };

  PUBLIC int csoundModuleInit_cmask(CSOUND *csound)
  {
    int status = 0;
    for(OENTRY *oentry = &oentries[0]; oentry->opname; oentry++)
      {
        status |= csound->AppendOpcode(csound, oentry->opname,
                                       oentry->dsblksiz, oentry->flags,
                                       oentry->thread,
                                       oentry->outypes, oentry->intypes,
                                       (int (*)(CSOUND*,void*)) oentry->iopadr,
                                       (int (*)(CSOUND*,void*)) oentry->kopadr,
                                       (int (*)(CSOUND*,void*)) oentry->aopadr);
      }
    return status;
  }
#ifndef INIT_STATIC_MODULES
  PUBLIC int csoundModuleCreate(CSOUND *csound)
  {
    return 0;
  }

  PUBLIC int csoundModuleInit(CSOUND *csound)
  {
      return csoundModuleInit_cmask(csound);
  }

  PUBLIC int csoundModuleDestroy(CSOUND *csound)
  {
    return 0;
  }
#endif
}

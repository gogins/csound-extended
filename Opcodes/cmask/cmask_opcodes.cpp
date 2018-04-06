/*
    cmask_opcodes.cpp

    Copyright (C) 2018 Michael Gogins

    This source code is for CMask opcodes in Csound.
    The CMask opcodes are licensed under the terms of 
    the Mozilla Public License, version 2.0.
 */
#include <OpcodeBase.hpp>
#include <cstdio>
#include <fstream>
#include <iostream>
#include <string>
#include <sstream>

// Because CMask sources have symbols whose names conflict with Csound (e.g.
// "GEN", and because I do not want to extensively change CMask sources, 
// we simply include all sources in this one compilation unit in a separate 
// namespace.

namespace cmask {
using namespace std;
#include "fileio.h"
#include "parser.h"
#include "utils.h"
#include "event.h"
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

/**
 * Encapsulates all functionality of Andre Bartetzki's CMask program 
 * for algorithmic composition into one i-rate Csound opcode.
 * THe cmask opcode receives a string containing the text of a CMask 
 * parameter file, and returns a string containing the generated Csound score.
 * If the optional play_immediately parameter is true, the generated score is 
 * immediately played.
 */
class CMask : public csound::OpcodeBase<CMask>
{
public:
  // Csound opcode outputs.
  STRINGDAT *score;
  // Csound opcode inputs.
  STRINGDAT *parameters_text;
  MYFLT *play_immediately;
  // State.
  cmask::scanner scanner_;

  int init(CSOUND *csound)
  {
    static bool initialized = false;
    if (!initialized) {
        cmask::frandinit();
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
    parameters_file << parameters_text->data << std::endl;
    parameters_file.close();
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
    score->data = csound->Strdup(csound, (char *)score_text.c_str());
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
        (char*)"So",
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

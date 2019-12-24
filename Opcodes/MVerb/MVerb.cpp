/*
    mverb.cpp

    Copyright (C) 2006 Michael Gogins

    This file is part of Csound.

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
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
    02110-1301 USA
 */
#include "OpcodeBase.hpp"
#include <Gamma/Gamma.h>
#include <Gamma/Analysis.h>
#include <Gamma/Filter.h>
#include <Gamma/Delay.h>
#include <Gamma/Scheduler.h>

using namespace csound;

#include <cmath>

/**
 * Implements Jon Christopher Nelson's MVerb opcode in C++.
 * 
 * MVerb is a plugin that is based on a modified five-by-five 2D waveguide 
 * mesh developed in Csound within the Cabbage framework. MVerb is highly 
 * flexible and can generate compelling and unique reverberation effects 
 * ranging from traditional spaces to infinite morphing spaces or the 
 * simulation of metallic plates or cymbals. The plugin incorporates a 10-band
 * parametric EQ for timbral control and delay randomization to create more 
 * unusual effects.
 *
 * opcode EQ,a,a ;1 audio out, 1 audio in ;get k-rate data through chnget arguments
 * opcode meshEQ,aaaa,aaaaak ;4 audio outs and 5 audio ins one k-rate in for FB value, use for boundary nodes
 * opcode randomdel,a,a
 *
 * This opcode requires the Gamma library for audio signal processing.
 *
 */
 
struct Balance : gam::
    gam::EnvFollow rms_source;
    gam::EnvFollow rms_comparator;
};

struct EQ {
    gam::Biquad<> pareq;
};

struct MeshEQ {
    gam::Comb<> delay_up;
    gam::Comb<> delay_right;
    gam::Comb<> delay_down;
    gam::Comb<> delay_left;
    Balance balance;
    gam::BlockDC<> dcblock;
};


struct RandomDelay {
    gam::Comb delay;
};

struct Preset {
    
};

class MVerb  : public OpcodeBase<MVerb>
{
public:
  // Outputs.
  // Inputs.
  // State.
  gam::Scheduler scheduler;
  MYFLT onedrms;
  MYFLT dbfs;
  MVerb() {}
  int init(CSOUND *csound)
  {
      // Order of processing:
      // 25 Random delays
      // 25 Multitap delays
      // 5 MeshEQs each with
      //    4 Variable delays
      //    4 EQs
      //        10 Biquad filters
      //        2 DCBlock
      //        2 Balance
      // 2 DCBlock
      
      return OK;
  }
  int kontrol(CSOUND *csound)
  {
      return OK;
  }
};

extern "C" {

  PUBLIC int csoundModuleInit_mverb(CSOUND *csound)
  {
      int status = csound->AppendOpcode(csound,
                                        (char*)"MVerb",
                                        sizeof(MVerb),
                                        0,
                                        3,
                                        (char*)"k",
                                        (char*)"kio",
                                        (int(*)(CSOUND*,void*)) MVerb::init_,
                                        (int(*)(CSOUND*,void*)) MVerb::kontrol_,
                                        (int (*)(CSOUND*,void*)) 0);
      return status;
  }

#ifndef INIT_STATIC_MODULES
  PUBLIC int csoundModuleCreate(CSOUND *csound)
  {
      return 0;
  }

  PUBLIC int csoundModuleInit(CSOUND *csound)
  {
      return csoundModuleInit_mverb(csound);
  }

  PUBLIC int csoundModuleDestroy(CSOUND *csound)
  {
      return 0;
  }
#endif
}

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
 */
 
/**
 * The UnitGenerator class represents one node in a fixed signal flow graph.
 * Initialization and ticking are done depth-first, at each node 
 * delegating to a node-local version of the function, one block per tick. 
 * Processing is performed in the node_tick function, one frame per tick.
 */
struct UnitGenerator {
    std::vector<double> input;
    std::vector<double> output;
    std::vector<UnitGenerator *> sources;
    virtual void add_source(UnitGenerator &source) {
        sources.push_back(&source);
    };
    virtual void graph_initialize(int frames_per_sample, int frames_per_second) {
        input.resize(frames_per_sample);
        output.resize(frames_per_sample);
        for (UnitGenerator *source : sources) {
            source->graph_initialize(frames_per_sample, frames_per_second);
        }
        node_initialize(frames_per_sample, frames_per_second);
    };
    virtual void node_initialize(int frames_per_sample, int frames_per_second) {
    };
    virtual void graph_tick(int frames_per_sample, int frames_per_second) {
        for (UnitGenerator *source : sources) {
            source->graph_tick(frames_per_sample, frames_per_second);
        }
        for (frame = 0; frame < frames_per_sample; ++frame) {
            node_tick(output[frame], input[frame], frames_per_second);
        }
    };
    virtual void node_tick(double &out, double &in, int frames_per_second) {
        out = in;
    };
 };
struct ParametricEqualizer : UnitGenerator {
};
struct Equalizer : UnitGenerator {
    ParametricEqualizer bands[10];
    for (int i = 0; i < 10; ++i) {
        add_source(&bands[i]);
    }
};
struct RandomDelayLine : UnitGenerator {
};
struct MultitapDelayLine : UnitGenerator {
};
struct ScatteringJunction : UnitGenerator {
};
struct Preset {
    
};

struct ScatteringJunction {
    
};

class MVerb  : public OpcodeBase<MVerb>
{
public:
  // Outputs.
  audio_output;
  // Inputs.
  audio_input;
  // State.
  ScatteringJunction xxx;
  MYFLT onedrms;
  MYFLT dbfs;
  MVerb() {}
  int init(CSOUND *csound)
  {
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
                                        (char*)"ampmidid.k",
                                        sizeof(KAMPMIDID),
                                        0,
                                        3,
                                        (char*)"k",
                                        (char*)"kio",
                                        (int(*)(CSOUND*,void*)) KAMPMIDID::init_,
                                        (int(*)(CSOUND*,void*)) KAMPMIDID::kontrol_,
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

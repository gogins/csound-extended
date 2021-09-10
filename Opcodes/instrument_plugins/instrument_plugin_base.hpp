/*
    instrument_plugin.hpp:

    Copyright (C) 2005, 2009, 2017 by Istva Varga, Victor Lazzarini and
                                      Michael Gogins

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

#ifndef INSTRUMENT_PLUGIN_BASE_H
#define INSTRUMENT_PLUGIN_BASE_H

#include <OpcodeBase.pp>

/**
 * Template base class, or pseudo-virtual base class,
 * for writing Csound instruments as opcodes in C++.
 * Derive opcode classes like this:
 *
 * DerivedClass : public InstrumentPluginBase<DerivedClass>
 * {
 * public:
 *     int init();
 *     int kontrol();
 *     int noteoff();
 *     void deinit();
 * };
 *
 * All derivations of this class have an a-rate audio output 
 * variable with any number of channels, i.e. a vector audio signal.
 *
 * All derivations of this class have from 0 to 16 J type control
 * inputs. The number and meaning of these variables depends upon the 
 * derivation.
 *
 * All derivations of this class read the instrument pfields, which 
 * are assumed to be in this format:
 *
 * p1: instrument number, may be have a fractional part.
 * p2: onset time in beats (beats default to seconds).
 * p3: duration in beats.
 * p4: MIDI key number, may have a fractional part.
 * p5: MIDI velocity, may have a fractional part.
 * p6: Spatial position from front to back.
 * p7: Spatial position from left to right.
 * p8: Spatial position from bottom to top.
 * p9: Phase in radians.
 *
 * Additional instrument pfields may also be used, their meaning to be 
 * determined by the derived class.
 *
 * All derivations of this class may define any number of 
 * global control channels. These will be global variables 
 * that are exported as control channels using a naming convention:
 * 'g' for global + Csound type code + '_' + opcode name + '_' + parameter name,
 * e.g. 'gk_DerivedClass_level'. The meanings, units, and default values of 
 * these channels are determined by the derived class.
 *
 * If the named global variable is not found, it is created and exported as a 
 * control channel; if the named global variable is found, its definition is 
 * mapped to its name here.
 *
 * The pattern for using such derived classes is:
 *
 * instr DerivedClass
 * a_audio_output[] init 2
 * k_initial_something init 5
 * a_audio_output[] DerivedClass k_initial_something
 * outletv "output", a_audio_output
 * endin
 */

namespace csound
{

template<typename T>
class InstrumentPluginBase
{
public:
    int init(CSOUND *csound)
    {
        return NOTOK;
    }
    static int init_(CSOUND *csound, void *opcode)
    {
        return reinterpret_cast<T *>(opcode)->init(csound);
    }
    int kontrol(CSOUND *csound)
    {
        return NOTOK;
    }
    static int kontrol_(CSOUND *csound, void *opcode)
    {
        return reinterpret_cast<T *>(opcode)->kontrol(csound);
    }
     /**
      This is how to compute audio signals for normal opcodes:
      (1) Zero all frames from 0 up to but not including Offset.
      (2) Compute all frames from ksmps_offset up to but not including End.
      (3) Zero all frames from End up to but not including ksmps.
      Example from a C opcode:
      uint32_t offset = p->h.insdshead->ksmps_offset;
      uint32_t early  = p->h.insdshead->ksmps_no_end;
      uint32_t n, nsmps = CS_KSMPS;
      if (UNLIKELY(offset)) memset(p->r, '\0', offset*sizeof(MYFLT));
      if (UNLIKELY(early)) {
        nsmps -= early;
        memset(&p->r[nsmps], '\0', early*sizeof(MYFLT));
      }
      for (n = offset; n < nsmps; n++) {
        input1 = MYFLT2LRND(p->a[n]);
        p->r[n] = (MYFLT) (input1 >> input2);
      }
      So in C++ it should look like this (which is much easier to understand):
      int frameIndex = 0;
      for( ; frameIndex < kperiodOffset(); ++frameIndex) {
          asignal[frameIndex] = 0;
      }
      for( ; frameIndex < kperiodEnd(); ++frameIndex) {
          asignal[frameIndex] = compute();
      }
      for( ; frameIndex < ksmps(); ++frameIndex) {
          asignal[frameIndex] = 0;
      }
     */
     OPDS opds;
};

template<typename T>
class OpcodeNoteoffBase
{
public:
    int init(CSOUND *csound)
    {
        IGN(csound);
        return NOTOK;
    }
    static int init_(CSOUND *csound, void *opcode)
    {
        if (!csound->GetReinitFlag(csound) && !csound->GetTieFlag(csound)) {
            csound->RegisterDeinitCallback(csound, opcode,
                                           &OpcodeNoteoffBase<T>::noteoff_);
        }
        return reinterpret_cast<T *>(opcode)->init(csound);
    }
    int kontrol(CSOUND *csound)
    {
        IGN(csound);
        return NOTOK;
    }
    static int kontrol_(CSOUND *csound, void *opcode)
    {
        return reinterpret_cast<T *>(opcode)->kontrol(csound);
    }
    /**
      This is how to compute audio signals for normal opcodes:
      (1) Zero all frames from 0 up to but not including Offset.
      (2) Compute all frames from ksmps_offset up to but not including End.
      (3) Zero all frames from End up to but not including ksmps.
      Example from a C opcode:
      uint32_t offset = p->h.insdshead->ksmps_offset;
      uint32_t early  = p->h.insdshead->ksmps_no_end;
      uint32_t n, nsmps = CS_KSMPS;
      if (UNLIKELY(offset)) memset(p->r, '\0', offset*sizeof(MYFLT));
      if (UNLIKELY(early)) {
        nsmps -= early;
        memset(&p->r[nsmps], '\0', early*sizeof(MYFLT));
      }
      for (n = offset; n < nsmps; n++) {
        input1 = MYFLT2LRND(p->a[n]);
        p->r[n] = (MYFLT) (input1 >> input2);
      }
      So in C++ it should look like this (which is much easier to understand):
      int frameIndex = 0;
      for( ; frameIndex < kperiodOffset(); ++frameIndex) {
          asignal[frameIndex] = 0;
      }
      for( ; frameIndex < kperiodEnd(); ++frameIndex) {
          asignal[frameIndex] = compute();
      }
      for( ; frameIndex < ksmps(); ++frameIndex) {
          asignal[frameIndex] = 0;
      }
     */
    uint32_t kperiodOffset() const
    {
        return opds.insdshead->ksmps_offset;
    }
    uint32_t kperiodEnd() const
    {
        uint32_t end = opds.insdshead->ksmps_no_end;
        if (end) {
            return end;
        } else {
            return ksmps();
        }
    }
    uint32_t ksmps() const
    {
        return opds.insdshead->ksmps;
    }
    uint32_t output_arg_count()
    {
        return (uint32_t)opds.optext->t.outArgCount;
    }
    uint32_t input_arg_count()
    {
        return (uint32_t)opds.optext->t.inArgCount;
    }
    void log(CSOUND *csound, const char *format,...)
    {
        va_list args;
        va_start(args, format);
        if(csound) {
            csound->MessageV(csound, 0, format, args);
        } else {
            vfprintf(stdout, format, args);
        }
        va_end(args);
    }
    void warn(CSOUND *csound, const char *format,...)
    {
        if(csound) {
            if(csound->GetMessageLevel(csound) & WARNMSG) {
                va_list args;
                va_start(args, format);
                csound->MessageV(csound, CSOUNDMSG_WARNING, format, args);
                va_end(args);
            }
        } else {
            va_list args;
            va_start(args, format);
            vfprintf(stdout, format, args);
            va_end(args);
        }
    }
    int noteoff(CSOUND *csound)
    {
        return OK;
    }
    static int noteoff_(CSOUND *csound, void *opcode)
    {
        return reinterpret_cast<T *>(opcode)->noteoff(csound);
    }
    OPDS opds;
};

};

#endif

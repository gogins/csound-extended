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
    MYFLT pfield(int i) {
        auto pfields = (CS_VAR_MEM *)opds.insdshead->p1;
        // Pfields are 1-based, implicit pfield array is 0-based.
        auto pfield = pfields[i - 1];
        return pfield->value;
    }
    /**
     * Verify that the named channel exists as a global variable. If the
     * global variable is not found, it is created and exported as a 
     * control channel.
     */
    bool ensureChannel(CSOUND *csound, const char *channel_name) {
        void *global = csound->QueryGlobalVariable(csound, channel_name);
        if (global == nullptr) {
            char code_buffer[0x512];
            snprintf(code_buffer, 0x512, "%s chnexport \"%s\", 3\n", channel_name, channel_name);
            auto result = csound->EvalCode(csound, code_buffer);
            return result;
        } else {
            return true;
        }
    }
    /**
     * Receives a k-rate value from the named control channel. If the channel 
     * does not exist, it is created as a global variable and exported as a 
     * control channel.
     */
    MYFLT receiveK(CSOUND *csound, const char *name) {
        auto result = ensureChannel(csound, name);
        int error = 0;
        auto value = csound->GetControlChannel(csound, name, &error);
        if (error != OK) {
            csound->Message(csound, "Failed to get channel value for \"%s\" with error %d\n", error);
        }
        return value;
    }
    /**
     * Sends a k-rate value to the named control channel. If the channel does 
     * not exist, it is created as a global variable and exported as a control 
     * channel.
     */
    void sendK(CSOUND *csound, const char *name, MYFLT k_value) const {
        auto result = ensureChannel(csound, name);
        csound->SetControlChannel(csound, name, k_value);
    }
    /**
     * Receives a string value from the named control channel. If the channel 
     * does not exist, it is created as a global variable and exported as a 
     * control channel. Calling with a size less than 0 returns the size 
     * required for the buffer, which must be pre-allocated. The value is 
     * copied into the buffer, and the actual length of the value is returned.
     */
    int receiveS(CSOUND *csound, const char *name, char *buffer, int size) {
        auto result = ensureChannel(csound, name);
        if (size < 0) {
            auto size = csound->GetChannelDatasize(csound);
            return size;
        }
        csound->GetStringChannel(csound, name, buffer);
        return std::strnlen(buffer, size);
    }
    /** 
     * Sends a string value to the named control channel. If the channel does
     * not exist, it is created as a global variable and exported as a control 
     * channel.
     */
    void sendS(CSOUND *csound, const char *name, const char *value) const {
        ensureChannel(csound, name);
        csound->SetStringChannel(csound, name, value);
    }
    /**
     * Receives an audio value from the named control channel. If the channel 
     * does not exist, it is created as a global variable and exported as a 
     * control channel. The audio value is an array of ksmps MYFLT samples 
     * and is returned in the argument, which must be preallocated.
     */
    void receiveA(CSOUND *csound, const char *name, MYFLT *a_value) {
        ensureChannel(csound, name);
        csound->GetAudioChannel(csound, name, a_value);
    }
    /**
     * Sends an audio value to the named control channel. If the channel 
     * does not exist, it is created as a global variable and exported as a 
     * control channel. The audio value must be an array of ksmps MYFLT 
     * samples.
     */
    void sendA(CSOUND *csound, const char *name, MYFLT *a_value) const {
        ensureChannel(csound, name);
        csound->SetAudioChannel(csound, name, a_value);
    }
    /** 
     * Receives a frame of streaming phase vocoder data from the named 
     * control channel, e.g. as might be sent from a pvsout opcode. If the 
     * channel does not exist, it is created as a global variable and 
     * exported as a control channel. The PVS data is returned in the 
     * PVSDATEXT argument, which must be preallocated. An error is returned if 
     * the frame is not compatible with the channel.
     */
    int receivePVS(CSOUND *csound, const char *name, PVSDATEXT *frame) {
        ensureChannel(csound, name);
        auto result = csound->GetPvsChannel(csound, frame, name);
        return result;
    }
    /** 
     * Sends a frame of streaming phase vocoder data to the pvsin opcode 
     * that is using the named control channel. If the channel does not 
     * exist, it is created as a global variable and exported as a control 
     * channel. An error is returned if the frame is not compatible with 
     * the pvsin opcode.
     */
    int sendPVS(CSOUND *csound, const char *name, PVSDATEXT *frame) const {
        ensureChannel(csound, name);
        auto result = csound->SetPvsChannel(csound, frame, name);
        return result;
    }
    OPDS opds;
    ARRAYDAT *a_output;
};

};

#endif

/*
    csound_web_audio.hpp:

    Copyright (C) 2017 Michael Gogins

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
    02111-1307 USA

    As a special exception, if other files instantiate templates or
    use macros or inline functions from this file, this file does not
    by itself cause the resulting executable or library to be covered
    by the GNU Lesser General Public License. This exception does not
    however invalidate any other reasons why the library or executable
    file might be covered by the GNU Lesser General Public License.
*/

#ifndef __CSOUND_EMBIND_HPP__
#define __CSOUND_EMBIND_HPP__

#if defined(__GNUC__)
#if __cplusplus <= 199711L
  #error To use csound_webaudio.hpp you need at least a C++11 compliant compiler.
#endif
#endif

#include <csound.hpp>
#include <deque>
#include <emscripten/bind.h>
#include <emscripten/emscripten.h>
#include <emscripten/val.h>

extern "C" {
    int init_static_modules(CSOUND *csound);     
};

using namespace emscripten;

void csoundMessageCallback_(CSOUND *csound__, int attr, const char *format, va_list valist) {
    char buffer[0x1000];
    std::vsprintf(buffer, format, valist);
    ///std::printf("%s", buffer);
    EM_ASM_({
        console.log(UTF8ToString($0));
    }, buffer);    
}

struct MidiData {
	unsigned char status;
	unsigned char data1;
	unsigned char data2;
    unsigned char dummy;
};

static const int bytes_per_channel_message[8] = { 2, 2, 2, 2, 1, 1, 2, 0 };

/**
 * Provides a subset of the Csound C++ interface declared and defined in 
 * csound.hpp to the JavaScript context of Web browsers and other environments 
 * that support WebAssembly. Real-time audio is implemented using Web Audio. 
 * The semantics and run-time sequencing of this interface is almost identical 
 * to that of the corresponding subset of the Csound class.
 *
 * Member functions that take string parameters must, for Embind, take 
 * std::string instead of const char *. Please keep methods in alphabetical 
 * order by name.
 */
class CsoundEmbind : public Csound {
    std::deque<MidiData> midi_data_queue;
    bool midi_in_is_open = false;
public:
    virtual ~CsoundEmbind() {};
    CsoundEmbind() {
        Csound::SetHostData(this);
        Csound::SetMessageCallback(csoundMessageCallback_);
    };
    virtual int CompileCsd(const std::string &filename) {
        int result = 0;
        result |= init_static_modules(csound);
        result |= Csound::CompileCsd(filename.c_str());
        return result;
    }
    virtual int CompileCsdText(const std::string &csd) {
        int result = 0;
        result |= init_static_modules(csound);
        result |= Csound::CompileCsdText(csd.c_str());
        return result;
    }
    virtual int CompileOrc(const std::string &orc) {
        int result = 0;
        result |= init_static_modules(csound);
        result |= Csound::CompileOrc(orc.c_str());
        return result;
    }
    virtual MYFLT EvalCode(const std::string &orc) {
        return Csound::EvalCode(orc.c_str());
    }
    virtual MYFLT GetControlChannel(const std::string &name) {
        return Csound::GetControlChannel(name.c_str(), 0);
    }
    virtual std::string GetEnv(const std::string &name) {
        return GetEnv(name.c_str());
    }
    virtual std::string GetInputName_() {
        auto value = GetInputName();
        if (value == nullptr) {
            return "";
        }
        return value;;
    }
    virtual std::string GetOutputName_() {
        auto value = GetOutputName();
        if (value == nullptr) {
            return "";
        }
        return value;;
    }
    virtual val GetSpinView() {
        int count = GetKsmps() * GetNchnlsInput();
        return val(typed_memory_view(count, GetSpin()));
    }
    virtual val GetSpoutView() {
        int count = GetKsmps() * GetNchnls();
        return val(typed_memory_view(count, GetSpout()));
    }
    virtual std::string GetStringChannel(const std::string &name) {
        char buffer[0x200];
        Csound::GetStringChannel(name.c_str(), buffer);
        return buffer;
    }
    virtual void InputMessage(const std::string &sco) {
        Csound::InputMessage(sco.c_str());
    }
    virtual void Message(const std::string &message) {
        Csound::Message(message.c_str());
    }
    virtual void ReadScore(const std::string &sco) {
        Csound::ReadScore(sco.c_str());
    }
    virtual void SetChannel(const std::string &name, MYFLT value) {
        return Csound::SetChannel(name.c_str(), value);
    }
    virtual void SetStringChannel(const std::string &name, const std::string &value) {
        Csound::SetStringChannel(name.c_str(), (char *)value.c_str());
    }
    virtual void SetInput(const std::string &input) {
        return Csound::SetInput(input.c_str());
    }
    virtual int SetOption(const std::string &value) {
        return Csound::SetOption(value.c_str());
    }
    virtual void SetOutput(const std::string &output, const std::string &type_, const std::string &format) {
        return Csound::SetOutput(output.c_str(), type_.c_str(), format.c_str());
    }
    virtual int Start() {
        int result = 0;
        result |= Csound::Start();
        return result;
    }
    int MidiInOpenCallback(CSOUND *, void **, const char *) {
        midi_data_queue.clear();
        midi_in_is_open = true;
        return 0;
    }
    static int MidiInOpenCallback_(CSOUND *csound_, void **user_data, const char *device_name) {
        void *host_data = csoundGetHostData(csound_);
        return static_cast<CsoundEmbind *>(host_data)->MidiInOpenCallback(csound_, user_data, device_name);
    }
    int MidiInCloseCallback(CSOUND *, void *) {
        midi_in_is_open = false;
        midi_data_queue.clear();
        return 0;
    }
    static int MidiInCloseCallback_(CSOUND *csound_, void *user_data) {
        void *host_data = csoundGetHostData(csound_);
        return static_cast<CsoundEmbind *>(host_data)->MidiInCloseCallback(csound_, user_data);
    }
    int MidiReadCallback(CSOUND *, void *, unsigned char *buffer, int bytes_to_read) {
        int bytes_read = 0;
        while (midi_data_queue.empty() == false) {
            MidiData midi_data = midi_data_queue.front();
            int st = (int) midi_data.status;
            int d1 = (int) midi_data.data1;
            int d2 = (int) midi_data.data2;
            if (st < 0x80)
                goto next;
            if (st >= 0xF0 &&
                    !(st == 0xF8 || st == 0xFA || st == 0xFB ||
                      st == 0xFC || st == 0xFF))
                goto next;
            bytes_to_read -= (bytes_per_channel_message[(st - 0x80) >> 4] + 1);
            if (bytes_to_read < 0) break;
            // Write to Csound's MIDI input buffer.
            bytes_read += (bytes_per_channel_message[(st - 0x80) >> 4] + 1);
            switch (bytes_per_channel_message[(st - 0x80) >> 4]) {
            case 0:
                *buffer++ = (unsigned char) st;
                break;
            case 1:
                *buffer++ = (unsigned char) st;
                *buffer++ = (unsigned char) d1;
                break;
            case 2:
                *buffer++ = (unsigned char) st;
                *buffer++ = (unsigned char) d1;
                *buffer++ = (unsigned char) d2;
                break;
            }
    next:
            midi_data_queue.pop_front();
        }
        return bytes_read;
    }
    static int MidiReadCallback_(CSOUND * csound_, void *user_data, unsigned char *buffer, int bytes_to_read) {
        void *host_data = csoundGetHostData(csound_);        
        return static_cast<CsoundEmbind *>(host_data)->MidiReadCallback(csound_, user_data, buffer, bytes_to_read);
    }
    virtual void InitializeHostMidi() {
        Csound::SetHostImplementedMIDIIO(1);
        Csound::SetExternalMidiInOpenCallback(&CsoundEmbind::MidiInOpenCallback_);
        Csound::SetExternalMidiReadCallback(&CsoundEmbind::MidiReadCallback_);
        Csound::SetExternalMidiInCloseCallback(&CsoundEmbind::MidiInCloseCallback_);
    }
    virtual void MidiEventIn(unsigned char status, unsigned char data1, unsigned char data2) {
        if (midi_in_is_open) {
            MidiData midi_data;
            midi_data.status = status;
            midi_data.data1 = data1;
            midi_data.data2 = data2;
            midi_data_queue.push_back(midi_data);
        }
    }
};

/** 
 * Mounts the current directory in the JavaScript sandbox 
 * to a "/csound" mountpoint in the node filesystem 
 * (i.e., the physical directory that node.js or NW.js is 
 * running in). Does not work in browsers!
 */
void nodefs_mount() {
  EM_ASM(
    FS.mkdir('/csound');
    FS.mount(NODEFS, { root: '.' }, '/csound');
  );
}

/**
 * For the sake of backwards compatibility, all method names are declared with 
 * both initial capitals and camel case. Please keep bindings in 
 * alphabetical order. If the method is new, the case is the same as in the 
 * existing C/C++ API.
 */
EMSCRIPTEN_BINDINGS(csound_web_audio) {  
    function("nodefs_mount", &nodefs_mount);
    class_<Csound>("Csound")
        .function("Cleanup", &Csound::Cleanup)
        .function("cleanup", &Csound::Cleanup)
        .function("Get0dBFS", &Csound::Get0dBFS)
        .function("get0dBFS", &Csound::Get0dBFS)
        .function("GetAPIVersion", &Csound::GetAPIVersion)
        .function("getAPIVersion", &Csound::GetAPIVersion)
        .function("GetCurrentTimeSamples", &Csound::GetCurrentTimeSamples)
        .function("getCurrentTimeSamples", &Csound::GetCurrentTimeSamples)
        .function("GetKsmps", &Csound::GetKsmps)
        .function("getKsmps", &Csound::GetKsmps)
        .function("GetNchnls", &Csound::GetNchnls)
        .function("getNchnls", &Csound::GetNchnls)
        .function("GetNchnlsInput", &Csound::GetNchnlsInput)
        .function("getNchnlsInput", &Csound::GetNchnlsInput)
        .function("GetScoreOffsetSeconds", &Csound::GetScoreOffsetSeconds)
        .function("getScoreOffsetSeconds", &Csound::GetScoreOffsetSeconds)
        .function("GetScoreTime", &Csound::GetScoreTime)
        .function("getScoreTime", &Csound::GetScoreTime)
        .function("GetSr", &Csound::GetSr)
        .function("getSr", &Csound::GetSr)
        .function("GetVersion", &Csound::GetVersion)
        .function("getVersion", &Csound::GetVersion)
        .function("IsScorePending", &Csound::IsScorePending)
        .function("isScorePending", &Csound::IsScorePending)
        .function("Perform", select_overload<int()>(&Csound::Perform))
        .function("perform", select_overload<int()>(&Csound::Perform))
        .function("PerformKsmps", select_overload<int()>(&Csound::PerformKsmps))
        .function("performKsmps", select_overload<int()>(&Csound::PerformKsmps))
        .function("Reset", &Csound::Reset)
        .function("reset", &Csound::Reset)
        .function("RewindScore", &Csound::RewindScore)
        .function("rewindScore", &Csound::RewindScore)
        .function("SetHostImplementedAudioIO", &Csound::SetHostImplementedAudioIO)
        .function("SetScoreOffsetSeconds", &Csound::SetScoreOffsetSeconds)
        .function("setScoreOffsetSeconds", &Csound::SetScoreOffsetSeconds)
        .function("SetScorePending", &Csound::SetScorePending)
        .function("setScorePending", &Csound::SetScorePending)
        .function("Start", &Csound::Start)
        .function("start", &Csound::Start)
        .function("Stop", &Csound::Stop)
        .function("stop", &Csound::Stop)
        .function("TableGet", &Csound::TableGet)
        .function("tableGet", &Csound::TableGet)
        .function("TableLength", &Csound::TableLength)
        .function("tableLength", &Csound::TableLength)
        .function("TableSet", &Csound::TableSet)
        .function("tableSet", &Csound::TableSet)
        ;
    class_<CsoundEmbind, base<Csound> >("CsoundEmbind")
        .constructor<>()
        .function("CompileCsd", &CsoundEmbind::CompileCsd)
        .function("compileCsd", &CsoundEmbind::CompileCsd)
        .function("CompileCsdText", &CsoundEmbind::CompileCsdText)
        .function("compileCsdText", &CsoundEmbind::CompileCsdText)
        .function("CompileOrc", &CsoundEmbind::CompileOrc)
        .function("compileOrc", &CsoundEmbind::CompileOrc)
        .function("EvalCode", &CsoundEmbind::EvalCode)
        .function("evalCode", &CsoundEmbind::EvalCode)
        .function("GetChannel", &CsoundEmbind::GetControlChannel)
        .function("getChannel", &CsoundEmbind::GetControlChannel)
        .function("GetControlChannel", &CsoundEmbind::GetControlChannel)
        .function("getControlChannel", &CsoundEmbind::GetControlChannel)
        .function("GetEnv", &CsoundEmbind::GetEnv)
        .function("getEnv", &CsoundEmbind::GetEnv)
        .function("GetInputName", &CsoundEmbind::GetInputName_)
        .function("getInputName", &CsoundEmbind::GetInputName_)
        .function("GetOutputName", &CsoundEmbind::GetOutputName_)
        .function("getOutputName", &CsoundEmbind::GetOutputName_)
        .function("GetSpin", &CsoundEmbind::GetSpinView)
        .function("GetSpout", &CsoundEmbind::GetSpoutView)
        .function("GetStringChannel", &CsoundEmbind::GetStringChannel)
        .function("getStringChannel", &CsoundEmbind::GetStringChannel)
        .function("InitializeHostMidi", &CsoundEmbind::InitializeHostMidi)
        .function("InputMessage", &CsoundEmbind::InputMessage)
        .function("inputMessage", &CsoundEmbind::InputMessage)
        .function("Message", &CsoundEmbind::Message)
        .function("message", &CsoundEmbind::Message)
        .function("MidiEventIn", &CsoundEmbind::MidiEventIn)
        .function("ReadScore", &CsoundEmbind::ReadScore)
        .function("readScore", &CsoundEmbind::ReadScore)
        .function("SetChannel", &CsoundEmbind::SetChannel)
        .function("setChannel", &CsoundEmbind::SetChannel)
        .function("SetControlChannel", &CsoundEmbind::SetChannel)
        .function("setControlChannel", &CsoundEmbind::SetChannel)
        .function("SetInput", &CsoundEmbind::SetInput)
        .function("setInput", &CsoundEmbind::SetInput)
        .function("SetOption", &CsoundEmbind::SetOption)
        .function("setOption", &CsoundEmbind::SetOption)
        .function("Start", &CsoundEmbind::Start)
        .function("start", &CsoundEmbind::Start)
        ;
 }

#endif  // __CSOUND_WEBAUDIO_HPP__

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

#ifndef __CSOUND_WEB_AUDIO_HPP__
#define __CSOUND_WEB_AUDIO_HPP__

#if defined(__GNUC__)
#if __cplusplus <= 199711L
  #error To use csound_webaudio.hpp you need at least a C++11 compliant compiler.
#endif
#endif

#include <csound.hpp>
#include <emscripten/bind.h>

using namespace emscripten;

/**
 * Provides a subset of the Csound class interface declared and defined in 
 * csound.hpp to the JavaScript context of Web browsers that support 
 * WebAssembly. Real-time audio is implemented using Web Audio. The 
 * semantics and run-time sequencing of this interface is the same as that of 
 * the corresponding subset of the Csound class.
 *
 * Member functions that take string parameters must, for Embind, take 
 * std::string not const char *. Also, we need new member functions to set up 
 * the WebAudio driver. Please keep methods in alphabetical order by name.
 */
class CsoundWebAudio : public Csound {
public:
    virtual ~CsoundWebAudio() {};
    CsoundWebAudio() {};
    virtual int CompileCsd(const std::string &filename) {
        return Csound::CompileCsd(filename.c_str());
    }
    virtual int CompileCsdText(const std::string &csd) {
        return Csound::CompileCsdText(csd.c_str());
    }
    virtual int CompileOrc(const std::string &orc) {
        return Csound::CompileOrc(orc.c_str());
    }
    virtual MYFLT EvalCode(const std::string &orc) {
        return Csound::EvalCode(orc.c_str());
    }
    virtual int SetOption(const std::string &value) {
        return Csound::SetOption(value.c_str());
    }
    virtual MYFLT GetChannel(const std::string &name) {
        return GetControlChannel(name.c_str(), 0);
    }
    virtual std::string GetEnv(const std::string &name) {
        return GetEnv(name.c_str());
    }
    virtual std::string GetInputName_() {
        return GetInputName();
    }
    virtual std::string GetOutputName_() {
        return GetOutputName();
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
    virtual int Perform() {
        return Csound::Perform();
    }
    virtual void ReadScore(const std::string &sco) {
        Csound::ReadScore(sco.c_str());
    }
    virtual void SetChannel(const std::string &name, MYFLT value) {
        return Csound::SetChannel(name.c_str(), value);
    }
    virtual int SetStringChannel(const std::string &name, const std::string &value) {
        return Csound::SetStringChannel(name.c_str(), (char *)value.c_str());
    }
    virtual void SetInput(const std::string &input) {
        return Csound::SetInput(input.c_str());
    }
};

/**
 * For the sake of backwards compatibility, all method names are declared with 
 * both initial capitals and camel case. Please keep declarations in 
 * alphabetical order.
 */
EMSCRIPTEN_BINDINGS(csound_web_audio) {         
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
        .function("Reset", &Csound::Reset)
        .function("reset", &Csound::Reset)
        .function("RewindScore", &Csound::RewindScore)
        .function("rewindScore", &Csound::RewindScore)
        ;
    class_<CsoundWebAudio, base<Csound> >("CsoundWebAudio")
        .constructor<>()
        .function("CompileCsd", &CsoundWebAudio::CompileCsd)
        .function("compileCsd", &CsoundWebAudio::CompileCsd)
        .function("CompileCsdText", &CsoundWebAudio::CompileCsdText)
        .function("compileCsdText", &CsoundWebAudio::CompileCsdText)
        .function("CompileOrc", &CsoundWebAudio::CompileOrc)
        .function("compileOrc", &CsoundWebAudio::CompileOrc)
        .function("EvalCode", &CsoundWebAudio::EvalCode)
        .function("evalCode", &CsoundWebAudio::EvalCode)
        .function("SetOption", &CsoundWebAudio::SetOption)
        .function("setOption", &CsoundWebAudio::SetOption)
        .function("GetChannel", &CsoundWebAudio::GetChannel)
        .function("getChannel", &CsoundWebAudio::GetChannel)
        .function("GetControlChannel", &CsoundWebAudio::GetChannel)
        .function("getControlChannel", &CsoundWebAudio::GetChannel)
        .function("GetEnv", &CsoundWebAudio::GetEnv)
        .function("getEnv", &CsoundWebAudio::GetEnv)
        .function("GetInputName", &CsoundWebAudio::GetInputName_)
        .function("getInputName", &CsoundWebAudio::GetInputName_)
        .function("GetOutputName", &CsoundWebAudio::GetOutputName_)
        .function("getOutputName", &CsoundWebAudio::GetOutputName_)
        .function("GetStringChannel", &CsoundWebAudio::GetStringChannel)
        .function("getStringChannel", &CsoundWebAudio::GetStringChannel)
        .function("InputMessage", &CsoundWebAudio::InputMessage)
        .function("inputMessage", &CsoundWebAudio::InputMessage)
        .function("Message", &CsoundWebAudio::Message)
        .function("message", &CsoundWebAudio::Message)
        .function("ReadScore", &CsoundWebAudio::ReadScore)
        .function("readScore", &CsoundWebAudio::ReadScore)
        .function("SetChannel", &CsoundWebAudio::SetChannel)
        .function("setChannel", &CsoundWebAudio::SetChannel)
        .function("SetControlChannel", &CsoundWebAudio::SetChannel)
        .function("setControlChannel", &CsoundWebAudio::SetChannel)
        .function("SetInput", &CsoundWebAudio::SetInput)
        .function("setInput", &CsoundWebAudio::SetInput)
        ;
}

#endif  // __CSOUND_WEBAUDIO_HPP__

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

class CsoundWebAudio : public Csound {
public:
    virtual ~CsoundWebAudio() {};
    CsoundWebAudio() {};
    virtual int CompileCsd(const std::string &filename) {
        return Csound::CompileCsd(filename.c_str());
    }
    virtual int SetOption(const std::string &value) {
        return Csound::SetOption(value.c_str());
    }
};

EMSCRIPTEN_BINDINGS(csound_web_audio) {         ;    
    class_<CsoundWebAudio>("CsoundWebAudio")
        .constructor<>()
        .function("CompileCsd", &CsoundWebAudio::CompileCsd)
        .function("compileCsd", &CsoundWebAudio::CompileCsd)
        .function("SetOption", &CsoundWebAudio::SetOption)
        .function("setOption", &CsoundWebAudio::SetOption)
        ;
}

#endif  // __CSOUND_WEBAUDIO_HPP__
